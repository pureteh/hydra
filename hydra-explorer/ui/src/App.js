import { ReactComponent as RightArrow } from './right_arrow.svg';
import { ReactComponent as DownArrow } from './down_arrow.svg';
import './App.css';
import React, { useState, useEffect } from 'react';

const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
const baseUrl = protocol + '//' + window.location.host + window.location.pathname;

const initialState = {
  lastSlot: 7126228,
  countTxs: 0,
  lastBlock: '',
  heads: []
};

function Block({ block }) {
  return <a className='explore' href={'https://preview.cexplorer.io/block/' + block}>{block.substring(0, 30) + '...'}</a>;
}

function Stats({ lastSlot, lastBlock, countTxs, heads }) {
  return <div className='stats'>
    <h2>Basic stats</h2>
    <div className='content'>
      <div className='stat'><label>Slot</label><span>{lastSlot}</span></div>
      <div className='stat'><label>Block</label>
        <span>
          <Block block={lastBlock} />
        </span>
      </div>
      <div className='stat'><label>Txs</label><span>{countTxs}</span></div>
    </div>
    <div className='headsStat content'>
      <h2>Heads </h2>
      <div className='stat'><label>Open</label><span>{heads.open}</span></div>
      <div className='stat'><label>Closed</label><span>{heads.closed}</span></div>
      <div className='stat'><label>Total</label><span>{heads.total}</span></div>
      <div className='stat'><label>TVL</label><span>{heads.tvl}</span></div>
    </div>
  </div>;
}

function HeadId({ headId, tvl, open, closed, detailed, updateHead }) {
  const classNames = closed
    ? 'headId closed'
    : (open ? 'headId open' : 'headId');
  return <div className={classNames}>
    {
      detailed
        ? <span className='fold' onClick={() => updateHead(headId, (head) => ({ ...head, detailed: false }))}>
          <DownArrow />
        </span>
        : <span className='unfold' onClick={() => updateHead(headId, (head) => ({ ...head, detailed: true }))}>
          <RightArrow />
        </span >
    }
    <label>Head</label>
    <span>{headId}</span>
    <span className='tvl'>{tvl}</span>
    {closed ? <span className='deadline'>{closed.toISOString()}</span> : null}
  </div >;
}

function TxId({ txId }) {
  return <div className='txId'><label>TxId</label>
    <span>
      <a className='explore' href={'https://preview.cexplorer.io/tx/' + txId}>{txId.substring(0, 30) + '...'}</a>
    </span>
  </div>;
};

function PointRef({ point }) {
  return <div className='point'>
    <div className='block'><label>Block</label><span><Block block={point.blockHash} /></span></div>
    <div className='slot'><label>Slot</label><span>{point.slot}</span></div>
  </div>;
};

function Party({ hydra, cardano }) {
  return <tr className='party'><td>{hydra.substring(0, 40) + '...'}</td><td>{cardano.substring(0, 40) + '...'}</td></tr>
};

function Parties({ parties }) {
  return <div>
    <h3>Parties</h3>
    <table className='parties'>
      <thead>
        <tr><th>Hydra Key</th><th>Cardano Key Hash</th></tr>
      </thead>
      <tbody>
        {parties.map((e) => {
          return (<Party hydra={e.hydraKey} cardano={e.cardanoKey} />);
        })}
      </tbody>
    </table>
  </div>;
};

function Commit({ commit }) {
  return <tr className='commit'>
    <td className='vkey'>{commit.vkey.substring(0, 20) + '...'}</td>
    <td className='totalCommitted'>{commit.totalCommitted}</td>
    <td>
      <a className='explore' href={'https://preview.cexplorer.io/tx/' + commit.txId}>{commit.txId.substring(0, 20) + '...'}</a>
    </td>
    <td classsName='slot'>{commit.slot}</td>
    <td classsName='blockHash'>
      <a className='explore' href={'https://preview.cexplorer.io/block/' + commit.blockHash}>{commit.blockHash.substring(0, 20) + '...'}</a>
    </td>
  </tr>;
}

function Commits({ commits }) {
  return <div>
    <h3>Commits</h3>
    <table className='commits'>
      <thead>
        <tr><th>Key</th><th>TVL</th><th>Tx. Id.</th><th>Slot</th><th>Block</th></tr>
      </thead>
      <tbody>
        {commits.map((c) => <Commit commit={c} />)}
      </tbody>
    </table>
  </div>;
}

function totalValueLocked(head) {
  return head.commits.reduce((c, t) => t.totalCommitted + c, 0);
}

function Head({ head, updateHead }) {
  const parties = head.parties.map((e, i) => {
    return { hydraKey: e.vkey, cardanoKey: head.cardanoKeyHashes[i] }
  });
  return <div className="head">
    <HeadId headId={head.headId} detailed={head.detailed} open={head.open} closed={head.closed} updateHead={updateHead} tvl={totalValueLocked(head)} />
    {
      head.detailed ? <> <TxId txId={head.txId} />
        <PointRef point={head.point} />
        <Parties parties={parties} />
        <Commits commits={head.commits} />
      </> : null
    }
  </div>;
}

/** Display the list of current heads along with their status
*/
function Heads({ heads, updateHead }) {
  return <div className='heads'>
    {heads.map((e) => <Head key={e.headId} head={e} updateHead={updateHead} />)}
  </div>;
}

const SOCKET_RECONNECTION_TIMEOUT = 30;

const webSocket = new WebSocket(baseUrl);

function App() {
  const [ws, setWs] = useState(webSocket);
  const [socketUrl, setSocketUrl] = useState(baseUrl + '/' + initialState.lastSlot);
  const [explorerState, setExplorerState] = useState(initialState);

  /** Update a single head identified by its `headId`
   */
  function updateHead(headId, updateFn) {
    const newHeads = explorerState.heads.map((head) => {
      if (head.headId === headId) {
        return updateFn(head);
      } else {
        return head;
      }
    });
    setExplorerState({ ...explorerState, heads: newHeads });
  }

  function addCommits(msg) {
    return (head) => {
      if (head.headId === msg.headCommit.headId) {
        const totalCommitted = msg.headCommit.committed.reduce((t, c) => t + c.value.lovelace, 0);
        const commit = {
          vkey: msg.headCommit.party.vkey,
          totalCommitted,
          txId: msg.txId,
          blockHash: msg.point.blockHash,
          slot: msg.point.slot
        };
        return { ...head, commits: [...head.commits, commit] }
      } else {
        return head;
      }
    }
  }


  function addCollectCom(msg) {
    return (head) => {
      if (head.headId === msg.headCollectCom.headId) {
        return { ...head, open: msg.headCollectCom.utxoHash }
      } else {
        return head;
      }
    }
  }

  function closeHead(msg) {
    return (head) => {
      if (head.headId === msg.headClose.headId) {
        const posixTime = msg.headClose.closeContestationDeadline;
        return { ...head, closed: new Date(posixTime) }
      } else {
        return head;
      }
    }
  }

  function updateState(msg) {
    return (state) => {
      switch (msg.tag) {
        case 'HeadInit':
          return {
            ...state,
            heads: [{ ...msg.headInit, commits: [], point: msg.point, txId: msg.txId }, ...state.heads]
          };

        case "HeadCommit":
          return {
            ...state,
            heads: state.heads.map(addCommits(msg))
          };

        case "HeadOpen":
          return {
            ...state,
            heads: state.heads.map(addCollectCom(msg))
          };

        case "HeadClose":
          return {
            ...state,
            heads: state.heads.map(closeHead(msg))
          };

        case 'Forward':
          return {
            ...state,
            countTxs: state.countTxs + 1,
            lastSlot: msg.point.slot,
            lastBlock: msg.point.blockHash
          };
        default:
          console.log("irrelevant message", msg);
          return state;
      }
    }
  }

  useEffect(() => {
    const onClose = () => {
      setTimeout(() => {
        setWs(new WebSocket(socketUrl));
      }, SOCKET_RECONNECTION_TIMEOUT);
    };

    const onMessage = (e) => {
      const msg = JSON.parse(e.data);
      setExplorerState(updateState(msg));
    };

    ws.addEventListener("close", onClose);
    ws.addEventListener("message", onMessage);

    return () => {
      ws.removeEventListener("close", onClose);
      ws.removeEventListener("message", onMessage);
    };
  }, [ws, setWs, socketUrl, setSocketUrl]);

  // const handleClickChangeSocketUrl = useCallback(
  //   () => setSocketUrl(baseUrl + '/' + lastSlot),
  //   [lastSlot]
  // );

  function headsStats(heads) {
    const total = heads.length;
    const closed = heads.filter((h) => h.closed).length;
    const open = heads.filter((h) => h.open && !h.closed).length;
    const tvl = heads.reduce((t, h) => t + totalValueLocked(h), 0);

    return { total, closed, open, tvl };
  }

  return (
    <div className="App">
      <header className="App-header">
        <h1>Hydra Observer</h1>
      </header>
      <div className="App-content">
        <Stats lastSlot={explorerState.lastSlot}
          lastBlock={explorerState.lastBlock}
          countTxs={explorerState.countTxs}
          heads={headsStats(explorerState.heads)}
        />
        <Heads heads={explorerState.heads} updateHead={updateHead} />
      </div>
    </div >
  );
}

export default App;
