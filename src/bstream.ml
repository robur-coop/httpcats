(* copied from miou *)
type 'a t = {
    buffer: 'a array
  ; mutable rd_pos: int
  ; mutable wr_pos: int
  ; lock: Miou.Mutex.t
  ; non_empty: Miou.Condition.t
  ; non_full: Miou.Condition.t
}

let create size v =
  let lock = Miou.Mutex.create () in
  let non_empty = Miou.Condition.create () in
  let non_full = Miou.Condition.create () in
  { buffer= Array.make size v; lock; rd_pos= 0; wr_pos= 0; non_empty; non_full }

let put t data =
  Miou.Mutex.lock t.lock;
  while (t.wr_pos + 1) mod Array.length t.buffer = t.rd_pos do
    Miou.Condition.wait t.non_full t.lock
  done;
  t.buffer.(t.wr_pos) <- data;
  t.wr_pos <- (t.wr_pos + 1) mod Array.length t.buffer;
  Miou.Condition.signal t.non_empty;
  Miou.Mutex.unlock t.lock

let get t =
  Miou.Mutex.lock t.lock;
  while t.wr_pos = t.rd_pos do
    Miou.Condition.wait t.non_empty t.lock
  done;
  let data = t.buffer.(t.rd_pos) in
  t.rd_pos <- (t.rd_pos + 1) mod Array.length t.buffer;
  Miou.Condition.signal t.non_full;
  Miou.Mutex.unlock t.lock;
  data
