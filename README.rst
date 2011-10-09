========================================
Dizzee - Subprocess management for Emacs
========================================

Dizzee is a pleasant way to manage your project's subprocesses in Emacs.

You have a project.

In order to get an instance running and start working, you have to manually launch say... 4 processes in 4 different shells.

This is *Not Fun*.

Thankfully it is also a definable, repeatable process - which means that we can *Use Programming*.

At worst, this is *More Fun* than doing it yourself every time.

Services
========

The fundamental abstraction in Dizzee is that of a `Service`.

A service is an individual process such as you would otherwise launch in a shell - e.g. serving the contents of a directory on localhost port 7878::

    you@yourbox $ python -m SimpleHTTPServer 7878

In production you might (read *really, really should*) have a whole webserver to take care of that sort of thing, but while working you take a simpler route.

With Dizzee you could define a `service` called *my-static* to turn this process into a simple::

    M-x my-static-start


To define this as a Dizzee service you would add the following to your .emacs::

   (dz-defservice my-static "python"
                            :args '("-m" "SimpleHTTPServer" "7878")
                            :cd "/home/you/your-awesome-project/static")


This provides you with the following functions that you can bind to shortcuts or call with M-x::

* my-static-start
* my-static-stop
* my-static-restart


Arguments to dz-defservice
--------------------------

Positional:

* Service   - (symbol) The name of the service
* Command   - (string) The executable to run

Keyword:

* :cd       - (string) The directory in which you would like the service to run
* :args     - (list of strings) The arguments you would like to be passed to the executable

Service Groups
==============

With `Service Groups` things get even *More Fun*. `Service Groups` allow you to define a group of conceptually related `Services` that you will want to launch simultaneously.

By Way Of Example
-----------------

Let's say you are plausibly working on a "Server" that will talk to a "Client". Having defined the `Sevices` my-server and my-client you can then define a `Service Group`::

   (dz-defservice-group my-project
                        my-server
                        my-client)


This will provide the M-x functions

* my-project-start     - Starts all the services
* my-project-stop      - Stops all the services
* my-project-restart   - Restarts all the services


Reloading
=========

Dizzee also provides a reloader to restart your service when you make changes to the source.
More complete docs to follow.
For now C-h f dz-register-reload

Installation
============

Use el-get with the github repo as a git source.

Or, if you insist, (*grumbles...*) download dizzee.el and::

    (require 'dizzee)


Bugs
====

Use the Github tracker https://github.com/davidmiller/dizzee/issues
