---
slug: 23
title: |
  23. Resource-based API
authors: []
tags: [Draft]
---

## Status

Draft

## Context

- [ADR-3](/adr/3) concluded that a full-duplex communication channels are
  desirable to interact with a _reactive_ system.

- The Client API communicates several types of messages to clients. Currently
  this ranges from node-level `PeerConnected`, over head-specific `HeadIsOpen`
  to messages about transactions like `TxValid`. These messages are all of type
  `ServerOutput`.
- As [ADR-15](/adr/15) also proposes, some clients may not need (have) access to
  administrative information.

- Clients can currently retrieve the whole history of these messages or
  opt-out - all or nothing.

- Currently there exists a `GetUTxO` query-like `ClientInput`, which will
  respond with the `GetUTxOResponse` `ServerOutput`.

- Users are not satisfied with this basic query and request:

  - parameters to filter for example on specific addresses
  - choose between `json` or binary (`cbor`) output of `UTxO`

- Similarly, the all or nothing approach on the history of messages is not
  serving well. For example:

  - connect to the node and check whether the head is open already or wait for
    it to be open

  - inclusion of the whole `UTxO` in the head is not always desirable

## Decision

- Do not add a general purpose querying interface (e.g. graphql) to the
  `hydra-node` and stay true to [ADR-3](/adr/3) of using duplex communication
  channels (using websockets).

- Create an API resource model to also compartmentalize the domain into topics
  on the API layer.

  - Map resources to HTTP paths and use a (likely local) websocket for each
    topic.

  - `/node` contains node-specific messages

    - sends outputs: `PeerConnected`, `PeerDisconnected`

  - `/head` contains head-specific messages

    - sends outputs: `HeadIsInitializing`, `Committed`, `HeadIsOpen`,
      `HeadIsClosed`, `HeasIsContested`, `ReadyToFanout`, `HeadIsAborte`,
      `HeadIsFinalized`, `RolledBack`, `PostTxOnChainFailed`

    - does NOT contain head internal messages about transactions and snapshots

    - accepts inputs: `Init`, `Abort`, `Close`, `Contest`, `Fanout`

  - `/head/ledger` contains messages about ledger in an open head

    - sends outputs: `TxValid`, `TxInvalid`, `SnapshotConfirmed`

    - accepts inputs: `NewTx`

- Drop `GetUTxO` and `GetUTxOResponse` messages

## Consequences

## TBD/TODO

- Send history and existing messages vs. latest state + updates on resources
- Outline some details how this could be implemented / where data is stored in the node.
- Keep `/` for backward compatibility?
