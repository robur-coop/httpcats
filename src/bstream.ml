type 'a t = {
    buffer: 'a option array
  ; mutable rd_pos: int
  ; mutable wr_pos: int
  ; mutable closed: bool
  ; mutable halted: bool
  ; lock: Miou.Mutex.t
  ; non_empty: Miou.Condition.t
  ; non_full: Miou.Condition.t
}

let create size : 'a t =
  let lock = Miou.Mutex.create () in
  let non_empty = Miou.Condition.create () in
  let non_full = Miou.Condition.create () in
  {
    buffer= Array.make size None
  ; lock
  ; rd_pos= 0
  ; wr_pos= 0
  ; closed= false
  ; halted= false
  ; non_empty
  ; non_full
  }

let close t =
  Miou.Mutex.protect t.lock @@ fun () ->
  t.closed <- true;
  Miou.Condition.signal t.non_empty

let halt t =
  Miou.Mutex.protect t.lock @@ fun () ->
  t.halted <- true;
  Miou.Condition.signal t.non_empty

let put t data =
  Miou.Mutex.protect t.lock @@ fun () ->
  if not (t.closed || t.halted) then (
    while (t.wr_pos + 1) mod Array.length t.buffer = t.rd_pos do
      Miou.Condition.wait t.non_full t.lock
    done;
    (* do nothing if stream was closed or halted while waiting *)
    if not (t.closed || t.halted) then (
      t.buffer.(t.wr_pos) <- Some data;
      t.wr_pos <- (t.wr_pos + 1) mod Array.length t.buffer;
      Miou.Condition.signal t.non_empty))

let get t =
  Miou.Mutex.protect t.lock @@ fun () ->
  while t.wr_pos = t.rd_pos && not (t.closed || t.halted) do
    Miou.Condition.wait t.non_empty t.lock
  done;
  if t.halted || (t.closed && t.rd_pos = t.wr_pos) then None
  else
    let data = t.buffer.(t.rd_pos) in
    t.buffer.(t.rd_pos) <- None;
    t.rd_pos <- (t.rd_pos + 1) mod Array.length t.buffer;
    Miou.Condition.signal t.non_full;
    data
