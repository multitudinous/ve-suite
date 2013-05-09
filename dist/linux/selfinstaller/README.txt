===For Linux systems: How to run VE-Suite 3===

You have two options for running VE-Suite 3:

 1. Desktop Mode
 2. Cluster Mode

(1) Desktop Mode

VE-Suite provides a convenience script for running in desktop mode. For
example, in a shell, run:

$ cd <VES install root>
$ ./bin/launch-ves_xplorer-desktop.sh

where <VES install root> is the location where you installed VE-Suite.

(2) Cluster Mode

Before you can launch VE-Suite on a cluster, you must create a configuration
file that describes your cluster setup. An example configuration file is
available at <VES install root>/share/vesuite/ves3-cluster-config.example.

VE-Suite provides a convenience script for running on a cluster. The script
is located at <VES install root>/bin/ves-cluster-control.sh.

To launch VE-Suite in cluster mode:

$ cd <VES install root>
$ ./bin/ves-cluster-control.sh -c <config file> -a launch -n <named VR Juggler config>

where <config file> is the path to your cluster config file, and
<named VR Juggler config> refers to a named configuration from your
config file. For example, if your config file is in your home directory
at ~/cluster-config and you want to use a VRJ config named "main", do:

$ ./bin/ves-cluster-control.sh -c ~/cluster-config -a launch -n main

If you get into trouble and need to stop VE-Suite, or VE-Suite doesn't
exit cleanly, do:

$ ./bin/ves-cluster-control.sh -c ~/cluster-config -a kill
