# Benchmark Protocol

This document describes the methodology used to benchmark HTTP server
implementations. The goal is to compare throughput (requests/second) and
latency across several OCaml HTTP server stacks and nginx as a baseline.

## Implementations under test

| Name | Stack | Runtime | Binary |
|------|-------|---------|--------|
| **httpcats** | [httpcats](https://github.com/robur-coop/httpcats) (h1/h2) | [miou](https://github.com/robur-coop/miou) | `smiou.exe` |
| **vif** | [vif](https://github.com/robur-coop/vif) (built on httpcats) | miou | `svif.exe` |
| **httpun+eio** | [httpun](https://github.com/anmonteiro/httpun) 0.2.0 | [eio](https://github.com/ocaml-multicore/eio) 1.3 | `seio.exe` |
| **nginx** | nginx/1.28.2 | native | `nginx` |

All OCaml servers respond to `GET /plaintext` with `Hello, World!`
(`text/plain`) and `GET /json` with `{"message":"Hello, World!"}`
(`application/json`). nginx only serves `/plaintext`.

All servers set `TCP_NODELAY` on accepted connections.

## Hardware

- **CPU**: AMD Ryzen 9 7950X — 16 cores / 32 threads (SMT enabled)
  - Base clock: 4.5 GHz, boost up to 5.88 GHz
  - L1d/L1i: 512 KiB each (16 instances), L2: 16 MiB (16 instances), L3: 64 MiB
    (2 × 32 MiB CCDs)
  - Single NUMA node (all 32 logical CPUs on node 0)
- **RAM**: 64 GB DDR5
- **OS**: Arch Linux, kernel `6.18.13-zen1-1-zen` (ZEN preemptive, SMP)
- **Compiler**: GCC 15.2.1

### CPU topology note

The Ryzen 9 7950X has 2 CCDs (Core Complex Dies), each with 8 cores. Cores 0-7
correspond to physical cores on CCD0 (logical CPUs 0-15 with SMT), and cores
8-15 to CCD1 (logical CPUs 16-31). The benchmark pins the server to cores 0-7
and wrk to cores 16-27, which places them on **separate CCDs**. This is
intentional: it avoids L3 cache contention between the server and the load
generator, at the cost of cross-CCD (Infinity Fabric) latency for any
inter-process communication — which is acceptable since they only communicate
over TCP loopback.

## Network configuration

All benchmarks run over **localhost** (`127.0.0.1:8080`). There is no physical
NIC involved; traffic goes through the kernel's loopback interface.

### Loopback interface

```
lo: mtu 65536, qdisc noqueue, state UNKNOWN
```

The default loopback MTU of 65536 bytes allows large TCP segments without
fragmentation.

### Relevant sysctl settings

| Parameter | Value | Note |
|-----------|-------|------|
| `net.core.somaxconn` | 4096 | Listen backlog upper bound |
| `net.ipv4.tcp_max_syn_backlog` | 4096 | SYN queue size |
| `net.ipv4.tcp_fin_timeout` | 60 | Default FIN-WAIT-2 timeout |
| `net.ipv4.tcp_tw_reuse` | 2 | Kernel default (reuse for loopback) |
| `net.ipv4.ip_local_port_range` | 32768–60999 | ~28k ephemeral ports |
| `net.core.rmem_max` | 4194304 | Max receive buffer (4 MiB) |
| `net.core.wmem_max` | 4194304 | Max send buffer (4 MiB) |
| `net.core.netdev_max_backlog` | 1000 | Default NIC backlog (not relevant for loopback) |

### File descriptor limit

```
ulimit -n = 524288
```

This is high enough to support all tested concurrency levels (up to 256
clients) without hitting file descriptor exhaustion.

### `TCP_NODELAY`

All servers explicitly set `TCP_NODELAY` on accepted connections to disable
Nagle's algorithm, ensuring that small HTTP responses are sent immediately
without waiting for additional data to coalesce.

## Load generator

- **wrk** version 4.2.0-3 (ArchLinux AUR package)
- Each test runs for **120 seconds** (`-d120s`)
- Each configuration is repeated **3 times** (3 attempts) to assess
  reproducibility

```
taskset -c 16-27 wrk -c <clients> -t <threads> -d120s http://localhost:8080/plaintext
```

## CPU pinning

To eliminate scheduling noise, the server and the load generator are pinned to
**disjoint CPU sets** using `taskset`:

| Component | CPU cores |
|-----------|-----------|
| Server | `0-7` (up to 8 cores) |
| wrk | `16-27` (12 cores) |

The cores in between (8-15) are left unused to avoid cache/interconnect
contention between the server and the load generator.

### Server launch commands

```sh
# httpcats (miou)
DOMAINS=<N-1> taskset -c 0-7 ./smiou.exe

# vif (miou)
DOMAINS=<N-1> taskset -c 0-7 ./svif.exe

# httpun+eio
DOMAINS=<N-1> taskset -c 0-7 ./seio.exe

# nginx
sudo taskset -c 0-7 nginx -p . -c nginx.conf
```

The `DOMAINS` environment variable controls the number of OCaml domains
(OS-level threads) used by the runtime. For miou-based servers, `DOMAINS=N-1`
means the server runs on N domains total (1 main domain + N-1 additional
domains). For eio, the value is passed to `Eio.Net.run_server
~additional_domains`.

For nginx, the number of worker processes is set in `nginx.conf`
(`worker_processes 8`) with `worker_cpu_affinity auto`.

## Test matrix

### Variable: number of domains (scalability)

With a fixed load of **12 clients / 12 threads**, the number of domains is
varied across `{1, 2, 4, 8}` to measure how each implementation scales with
parallelism.

### Variable: number of clients (concurrency)

With a fixed number of **8 domains**, the number of concurrent clients is
varied across `{12, 32, 64, 128, 256}` (with 12 wrk threads) and `{32, 64}`
(with 4 wrk threads) to measure behavior under increasing connection pressure.

## Metrics collected

From each wrk run, two metrics are recorded:
- **Average latency** and its **±σ percentage** (percentage of requests within
  ±1 standard deviation of the mean)
- **Average requests/second** and its **±σ percentage**

The ±σ percentage is reported directly by wrk as the `+/- Stdev` column. A
higher percentage indicates a more stable/predictable distribution.

## nginx configuration

nginx is configured for maximum raw throughput with the following key settings
(see `nginx.conf`):
- `worker_processes 8` with `worker_cpu_affinity auto`
- `worker_connections 32768`
- `access_log off` and `server_tokens off`
- `keepalive_requests 300000` (default is 100)
- `listen 8080 reuseport deferred fastopen=4096`
- Response: `return 200 "Hello, World!"`

This is intentionally an aggressive configuration to establish an upper bound
for throughput on this hardware.

## Reproducibility

To reproduce these benchmarks:
1. Build the OCaml servers:
   ```sh
   dune build smiou.exe seio.exe svif.exe
   ```
2. Start one server at a time, pinned to cores 0-7
3. Run wrk from a separate terminal, pinned to cores 16-28:
   ```sh
   taskset -c 16-27 wrk -c 12 -t 12 -d120s http://localhost:8080/plaintext
   ```
4. Repeat each configuration 3 times, recording the wrk summary output
5. Stop the server between configuration changes

Results are recorded in `BENCH.md` and visualized in `index.html`.
