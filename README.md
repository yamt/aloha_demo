aloha_demo
==========

This is an example to use [aloha](https://github.com/yamt/aloha).

[![Build Status](https://travis-ci.org/yamt/aloha_demo.png?branch=master)](https://travis-ci.org/yamt/aloha_demo)

How to run
----------

"make run" starts an OpenFlow controller which listens for OpenFlow
connections on port 6633.

    aloha_httpd_http  (cowboy protocol handler)
       |
    cowboy  (Unmodified cowboy)
       |
    ranch  (Unmodified ranch)
       |
    aloha_ranch  (ranch transport module for aloha_socket)
       |
    aloha_socket
       |
    aloha_tcp  (Pure Erlang TCP/IP stack)
       |
    aloha_ip
       |
    aloha_nic  (This virtual NIC has IP address 192.0.2.1)
       |
    aloha_datapath
       |
    aloha_ofc  (OpenFlow 1.3 Controller.  This uses of_protocol)
       |
    aloha_socket
       |
    gen_tcp/inet
       :
       : (OpenFlow channel)
       :
    OpenFlow 1.3 Switch  (Configure this switch to connect to our controller.)
       :
       : (Ethernet)
       :
    Some NIC

On the host which the above "Some NIC" belongs to, try:

    # ifconfig thesomenic 192.0.2.2
    # curl http://192.0.2.1:8080/
