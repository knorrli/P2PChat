# P2PChat - an experimental approach for P2P chatting in dynamic networks

### Installation

1. Get a working version of the TEDA environment running on your machine
2. Clone this repo inside the `apps` directory so that you have something like `/teda_light_2/apps/p2pchat`
3. That's it.

### Deployment
In the `p2pchat` dir, deploy the app to TEDA using the provided scripts

1. `$ ../../scripts/depl_app.sh p2pchat make hosts_alive.conf`
2. `$ ../../scripts/depl_enodes.sh p2pchat hosts_alive.conf`

Then run the app on the TEDA distributed environment with this command:

`$ ../../scripts/run.sh p2pchat "watcher:run()" hosts_alive.conf diufpc80.unifr.ch <$USER>`

