# P2PChat - an experimental approach for P2P chatting in dynamic networks

### Project Overview

We want to create a chat application that allows for Peer to Peer chatting in a
mesh network where nodes can connect/disconnect dynamically.
The chat application operates without a central master server, i.e. each
individual node routes received messages according to a set of routing
instructions which are updated whenever the network topology changes.

The chat application itself will only have a rudimentary feature set, and the
focus of the project will be on the following stuff:

1. Implemeting an efficient method for updating the routing information of
   affected nodes whenever a node connects or disconnects from the network
2. Implementing an optimized routing algorithm (based on existing shortest-path
   algorithms) for routing chat messages from one node to any other node in the
   network.


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

