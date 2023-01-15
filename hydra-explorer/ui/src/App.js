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

function Stats({ lastSlot, lastBlock, countTxs }) {
  return <div className='stats'>
    <h2>Basic stats</h2>
    <div className='stat'><label>Slot</label><span>{lastSlot}</span></div>
    <div className='stat'><label>Block</label>
      <span>
        <a href={'https://preview.cexplorer.io/block/' + lastBlock}>{lastBlock.substring(0, 30) + '...'}</a>
      </span>
    </div>
    <div className='stat'><label>Txs</label><span>{countTxs}</span></div>
  </div>;
}

function HeadId({ headId }) {
  return <div className='headId'><label>HeadId</label><span>{headId}</span></div>;
}

function Head(head) {
  return <div className="head">
    <HeadId headId={head.headId} />
  </div>;
}

/** Display the list of current heads along with their status
*/
function Heads({ heads }) {
  return <div className='heads'>
    {heads.map(Head)}
  </div>;
}

const SOCKET_RECONNECTION_TIMEOUT = 30;

const webSocket = new WebSocket(baseUrl);

function App() {
  const [ws, setWs] = useState(webSocket);
  const [socketUrl, setSocketUrl] = useState(baseUrl + '/' + initialState.lastSlot);
  const [heads, setHeads] = useState(initialState);

  function updateState(msg) {
    return (state) => {
      switch (msg.tag) {
        case 'HeadInit':
          return {
            ...state,
            heads: [{ headId: msg.headInit.headId }, ...state.heads]
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
      setHeads(updateState(msg));
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
        <img alt="Hydra Logo" width="100" src="/logo.png"></img>
        <h1>Hydra Observer</h1>
        <Stats lastSlot={heads.lastSlot}
          lastBlock={heads.lastBlock}
          countTxs={heads.countTxs}
        />
        <Heads heads={heads.heads} />
      </header>
    </div >
  );
}

export default App;
