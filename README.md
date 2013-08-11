aloha_demo
==========

An example to use aloha.

"make run" starts an OpenFlow controller which listens for OpenFlow
connections on port 6633.

    http server (cowboy/ranch)
       |
    aloha_socket
       |
    aloha_tcp
       |
    aloha_ip
       |
    aloha_nic  (this virtual NIC has IP address 192.0.2.1)
       |
    aloha_datapath
       |
    aloha_ofc  (this uses of_control)
       |
    aloha_socket
       :
       : (OpenFlow channel)
       :
    OpenFlow 1.3 Switch  (configure this switch to connect to our controller.)
       :
       : (ethernet)
       :
    some NIC

on the host which the above "some NIC" belongs to, try:

    # ifconfig thesomenic 192.0.2.2
    # curl http://192.0.2.1:8080/
