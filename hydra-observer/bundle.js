const protocol = window.location.protocol == "https:" ? "wss:" : "ws:";
const client = new WebSocket(protocol + "//" + window.location.host);

// Canvas
const display = document.getElementById('main');

const elem = (tag) => document.createElement(tag);
const text = (txt) => document.createTextNode(txt);

function displayInit(msg) {
  const initDiv = elem('div');
  initDiv.classList.add('head');

  const title = elem('span');
  title.classList.add('headId');
  title.appendChild(text(msg.headInit.headId));

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

  initDiv.append(title,slot,blockHash, parties);
  display.appendChild(initDiv);
}

// receive chain events through WS connection
client.addEventListener("message", e => {
  const msg = JSON.parse(e.data);
  switch (msg.tag) {
    case "HeadInit":
      console.log("New head seen", msg.headInit.headId);
      setTimeout(() => displayInit(msg), 0);

    case "HeadCommit":

    console.log("Commit  seen", JSON.stringify(msg));
//      setTimeout(() => displayCommit(msg), 0);

    default:
      console.log("irrelevant message", msg);
  }
});
