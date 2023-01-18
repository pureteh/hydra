import logo from './logo.svg';
import './App.css';
import React, { useState, useCallback, useEffect } from 'react';

const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
//const baseUrl = protocol + "//" + window.location.host + window.location.pathname;
const baseUrl = 'ws://explorer.hydra.family';

const initialState = {
  lastSlot: 7126228,
  countTxs: 0,
  lastBlock: '',
  heads: []
};

function Block({ block }) {
  return <a className='explore' href={'https://preview.cexplorer.io/block/' + block}>{block.substring(0, 30) + '...'}</a>
}

function Stats({ lastSlot, lastBlock, countTxs }) {
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
  </div>;
}

function HeadId({ headId, detailed, updateHead }) {
  return <div className='headId'><label>Head</label><span>{headId}</span>
    {
      detailed
        ? <span className='fold' onClick={() => updateHead(headId, (head) => ({ ...head, detailed: false }))}>
          &gt;
        </span>
        : <span className='unfold' onClick={() => updateHead(headId, (head) => ({ ...head, detailed: true }))}>
          v
        </span >
    }
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
  return <table className='parties'>
    <thead>
      <tr><th>Hydra Key</th><th>Cardano Key</th></tr>
    </thead>
    <tbody>
      {parties.map((e) => {
        return (<Party hydra={e.hydraKey} cardano={e.cardanoKey} />);
      })}
    </tbody>
  </table>;
};

function Head({ head, updateHead }) {
  const parties = head.parties.map((e, i) => {
    return { hydraKey: e.vkey, cardanoKey: head.cardanoKeyHashes[i] }
  });
  return <div className="head">
    <HeadId headId={head.headId} detailed={head.detailed} updateHead={updateHead} />
    {
      head.detailed ? <> <TxId txId={head.txId} />
        <PointRef point={head.point} />
        <Parties parties={parties} />
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

  function updateState(msg) {
    return (state) => {
      switch (msg.tag) {
        case 'HeadInit':
          return {
            ...state,
            heads: [{ ...msg.headInit, point: msg.point, txId: msg.txId }, ...state.heads]
          };


        // case "HeadCommit":
        //   setTimeout(() => displayCommit(msg), 0);

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

  return (
    <div className="App">
      <header className="App-header">
        <h1>Hydra Observer</h1>
      </header>
      <div className="App-content">
        <Stats lastSlot={explorerState.lastSlot}
          lastBlock={explorerState.lastBlock}
          countTxs={explorerState.countTxs}
        />
        <Heads heads={explorerState.heads} updateHead={updateHead} />
      </div>
    </div >
  );
}

export default App;
