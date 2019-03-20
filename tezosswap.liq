[%%version 0.9]

type state = 
  | Empty
  | Initiator

type swap = {
  initTimestamp: timestamp;
  refundTime: nat;
  hashedSecret: bytes;
  secret: bytes option;
  initiator: address;
  participant: address;
  value: tez;
  emptied: bool;
  state: state;
}    

type swaps = (bytes, swap) big_map

type storage = {
  swaps : (bytes, swap) big_map;
  nothing : unit
}

type initiate = {
  iRefundTime: nat;
  iHashedSecret: bytes;
  iParticipant: address;
  iMaster: bool
}

type redeem = {
  redeemHashedSecret: bytes;
  redeemSecret: bytes
}

type parameter = 
  | Initiate of initiate
  | Redeem of redeem
  | Refund of bytes
  
      
let isRefundable (hashedSecret: bytes)(swaps: swaps) =
  match Map.find hashedSecret swaps with
  | None -> false
  | Some swap ->
      if swap.emptied then 
        failwith "swap is emptied"
      else if Current.time() < swap.initTimestamp + int swap.refundTime then
        failwith "refundTime has not come"
      else true
        
let isRedeemable
    (hashedSecret: bytes)
    (secret: bytes)
    (swaps: swaps) =
  match Map.find hashedSecret swaps with
  | None -> false
  | Some swap ->
      if Bytes.length secret <> 32p then
        failwith "secret size is not correct" 
      else if (Crypto.sha256 (Crypto.sha256 secret) <> hashedSecret) then 
        failwith "secret is not correct" 
      else if Current.time() >= swap.initTimestamp + int swap.refundTime then
        failwith "refundTime has come"
      else if swap.emptied then 
        failwith "swap is emptied"
      else true

let isInitiatable
    (hashedSecret: bytes)
    (master: bool)
    (swaps: swaps) =
  match Map.find hashedSecret swaps with
  | None ->
      if not master then
        failwith "have to be master" 
      else true
  | Some swap ->
      if swap.emptied then
        failwith "hash is spent"
      else match swap.state with
        | Empty -> false
        | Initiator -> 
            if master then
              failwith "hash is initiated"
            else if Current.time() >= swap.initTimestamp + int swap.refundTime then
              failwith "refundTime has come"
            else true

let transfer
    (address : address)
    (amount : tez)=
  match (Contract.at address: UnitContract.instance option) with
  | None -> ([]: operation list)
  | Some participantContract ->
      [Contract.call participantContract amount ()]

      
let doInitiate
    (initiate : initiate)
    (storage : storage) =
  if not (isInitiatable initiate.iHashedSecret initiate.iMaster storage.swaps)
  then Current.failwith ();
  match Map.find initiate.iHashedSecret storage.swaps with
  | None ->
      let newSwap = {
        initTimestamp = Current.time();
        refundTime = initiate.iRefundTime;
        hashedSecret = initiate.iHashedSecret;
        initiator = Current.sender();
        participant = initiate.iParticipant;
        value = Current.amount();
        secret = (None: bytes option);
        emptied = false;
        state = Initiator;
      } in
      let swaps = Map.add initiate.iHashedSecret newSwap storage.swaps in
      let nothing = () in  
      let storage = {
        swaps;
        nothing
      } in
      ( ([]: operation list), storage )
  | Some swap ->
      let updatedSwap = swap.value <- swap.value + Current.amount() in
      let swaps = Map.update swap.hashedSecret (Some updatedSwap) storage.swaps in
      let nothing = () in  
      let storage = {
        swaps;
        nothing
      } in
      ( ([]: operation list), storage )
      
  
let doRedeem
    (redeem : redeem)
    (storage : storage) =
  if not (isRedeemable redeem.redeemHashedSecret redeem.redeemSecret storage.swaps)
  then Current.failwith ("hash is not found");
  match Map.find redeem.redeemHashedSecret storage.swaps with
  | None -> ( ([]: operation list), storage )
  | Some swap ->
      let u = match swap.state with
        | Empty -> ([] : operation list)
        | Initiator ->
            transfer swap.participant swap.value
      in
      let swap = swap.secret <- Some redeem.redeemSecret in
      let swap = swap.emptied <- true in
      let swap = swap.state <- Empty in
      let swaps = Map.add swap.hashedSecret swap storage.swaps in
      let nothing = () in
      let storage = {
        swaps;
        nothing
      } in
      ( u, storage )

let doRefund
    (hashedSecret : bytes)
    (storage : storage) =
  if not (isRefundable hashedSecret storage.swaps)
  then Current.failwith ("hash is not found");
  match Map.find hashedSecret storage.swaps with
  | None -> ( ([]: operation list), storage )
  | Some swap ->
      let u = match swap.state with
        | Empty -> ([] : operation list)
        | Initiator ->
            transfer swap.initiator swap.value
      in
      let swap = swap.emptied <- true in
      let swaps = Map.add swap.hashedSecret swap storage.swaps in
      let nothing = () in
      let storage = {
        swaps;
        nothing
      } in
      ( u, storage )

      
let%init storage: storage = {
  swaps = (BigMap : (bytes, swap) big_map);
  nothing = ()
}

let%entry main
    (parameter : parameter)
    (storage : storage) =
  match parameter with
  | Initiate initiate ->
      doInitiate initiate storage
  | Redeem redeem->
      doRedeem redeem storage
  | Refund hashedSecret->
      doRefund hashedSecret storage