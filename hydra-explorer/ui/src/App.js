import logo from './logo.svg';
import './App.css';
import React, { useState, useCallback, useEffect } from 'react';
import useWebSocket, { ReadyState } from 'react-use-websocket';

const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
//const baseUrl = protocol + "//" + window.location.host + window.location.pathname;
const baseUrl = 'ws://explorer.hydra.family';

const initialState = {
  'lastSlot': 0,
  'countTxs': 0,
  'lastBlock': ''
};

function Stats(props) {
  return <div className='stats'>
    <h2>Basic stats</h2>
    <div>Slot: <span className='slot'>{props.lastSlot}</span></div>
    <div>Block: <span className='block'>{props.lastBlock}</span></div>
    <div>Txs: <span className='txs'>{props.countTxs}</span></div>
  </div>;
}

function App() {
  //Public API that will echo messages sent to it back to the client
  const [socketUrl, setSocketUrl] = useState(baseUrl + '/');
  const [heads, setHeads] = useState(initialState);

  const { lastJsonMessage, readyState } = useWebSocket(socketUrl);

  function updateState(msg) {
    return (state) => {
      switch (msg.tag) {
        // case "HeadInit":
        //   setTimeout(() => displayInit(msg), 0);

        // case "HeadCommit":
        //   setTimeout(() => displayCommit(msg), 0);

        case "Forward":
          return {
            ...state, countTxs: state.countTxs + 1, lastSlot: msg.point.slot, lastBlock: msg.point.blockHash
          };
        default:
          console.log("irrelevant message", msg);
          return state;
      }
    }
  }

  useEffect(() => {
    if (lastJsonMessage !== null) {
      setHeads(updateState(lastJsonMessage));
    }
  }, [lastJsonMessage, setHeads]);

  // const handleClickChangeSocketUrl = useCallback(
  //   () => setSocketUrl(baseUrl + '/' + lastSlot),
  //   [lastSlot]
  // );

  const connectionStatus = {
    [ReadyState.CONNECTING]: 'Connecting',
    [ReadyState.OPEN]: 'Open',
    [ReadyState.CLOSING]: 'Closing',
    [ReadyState.CLOSED]: 'Closed',
    [ReadyState.UNINSTANTIATED]: 'Uninstantiated',
  }[readyState];

  return (
    <div className="App">
      <header className="App-header">
        <img alt="Hydra Logo" width="100" src="/logo.png"></img>
        <h1>Hydra Observer</h1>
        <span>The WebSocket is currently {connectionStatus}</span>
        <Stats lastSlot={heads.lastSlot}
          lastBlock={heads.lastBlock}
          countTxs={heads.countTxs}
        />
      </header>
    </div >
  );
}

export default App;
