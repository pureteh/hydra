const protocol = window.location.protocol == "https:" ? "wss:" : "ws:";

// Canvas
const display = document.getElementById('main');
var txCount = 0;
var lastSlot = 0;
var reconnectionDelay = 10;
const ONE_MINUTE = 60000;

const elem = (tag) => document.createElement(tag);
const text = (txt) => document.createTextNode(txt);

function displayInit(msg) {
  txCount++;

  const headDiv = elem('div');
  headDiv.classList.add('head');
  headDiv.id = 'head-' + msg.headInit.headId;

  const initDiv = elem('div');
  initDiv.classList.add('init');
  headDiv.append(initDiv);

  const title = elem('span');
  title.classList.add('headId');
  title.appendChild(text(msg.headInit.headId));

  const tid = elem('a');
  tid.classList.add('txId');
  tid.href = `https://preview.cexplorer.io/tx/${msg.txId}`;
  tid.target = '_blank';
  tid.appendChild(text(msg.txId));

  const slot = elem('span');
  slot.classList.add('slot');
  slot.appendChild(text(msg.point.slot));

  const blockHash = elem('span');
  blockHash.classList.add('blockHash');
  blockHash.appendChild(text(msg.point.blockHash));

  const parties = elem('table');
  parties.classList.add('parties');
  const hdr = elem('thead');
  parties.append(hdr);
  const hdrr = elem('tr');
  hdr.appendChild(hdrr);
  const th1 = elem('th');
  th1.appendChild(text('Hydra Key'));
  const th2 = elem('th');
  th2.appendChild(text('Cardano Key'));
  hdrr.append(th1, th2);

  const tbody = elem('tbody');
  parties.append(tbody);
  for(i = 0; i < msg.headInit.cardanoKeyHashes.length; i++) {
    const party = elem('tr');
    hdr.appendChild(party);
    const td1 = elem('td');
    td1.appendChild(text(msg.headInit.parties[i].vkey));
    const td2 = elem('td');
    td2.appendChild(text(msg.headInit.cardanoKeyHashes[i]));
    party.append(td1, td2);
    tbody.append(party);
  }

  initDiv.append(tid,slot,blockHash, parties);
  display.append(title, headDiv);
}

function displayCommit(msg) {
  txCount++;
  const headDiv = document.getElementById('head-' + msg.headCommit.headId);

  const commitDiv = elem('div');
  commitDiv.classList.add('commit');
  headDiv.append(commitDiv);

  const tid = elem('a');
  tid.classList.add('txId');
  tid.href = `https://preview.cexplorer.io/tx/${msg.txId}`;
  tid.target = '_blank';
  tid.appendChild(text(msg.txId));

  const slot = elem('span');
  slot.classList.add('slot');
  slot.appendChild(text(msg.point.slot));

  const blockHash = elem('span');
  blockHash.classList.add('blockHash');
  blockHash.appendChild(text(msg.point.blockHash));

  const party = elem('span');
  party.classList.add('party');
  party.appendChild(text(msg.headCommit.party.vkey));

  const committed = elem('span');
  committed.classList.add('committed');

  const totalCommitted = msg.headCommit.committed.reduce((t, c) => t + c.value.lovelace, 0);
  committed.appendChild(text(totalCommitted));

  commitDiv.append(tid,slot,blockHash,party,committed);
}

function displayForward(msg) {
  const slote = document.getElementById('lastSlot');
  const blocke = document.getElementById('lastBlock');
  const counte = document.getElementById('txCount');

  lastSlot = msg.point.slot;
  txCount++;

  slote.replaceChildren(text(lastSlot));
  blocke.replaceChildren(text(msg.point.blockHash));
  counte.replaceChildren(text(txCount));
}


// receive chain events through WS connection
const onMessage = function (e){
  const msg = JSON.parse(e.data);
  switch (msg.tag) {
  case "HeadInit":
    setTimeout(() => displayInit(msg), 0);

  case "HeadCommit":
    setTimeout(() => displayCommit(msg), 0);

  case "Forward":
    displayForward(msg);

  default:
    console.log("irrelevant message", msg);
  }
};

// Connect with websocket protocol using `lastSlot`
const onClose = function (e){
  setTimeout(() => {
    // 'exponential' backoff
    if (reconnectionDelay < ONE_MINUTE) {
      reconnectionDelay *= 2;
    } else {
      reconnectionDelay = 10;
    }
    const client = new WebSocket(protocol + "//" + window.location.host + window.location.pathname + '/'+ lastSlot);

    client.addEventListener("message", onMessage);
    client.addEventListener("close", onClose);
  }, reconnectionDelay);
};

onClose();
