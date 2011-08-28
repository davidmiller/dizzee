;;
;; test-dizzie.el
;;
;; Commentary:
;;
;; Helper code for testing the functionality of dizzie.el
;; At the moment this is not fully automated and requires some
;; manual interaction.
;; TODO: Solve that
;;
;; Running these scripts requires functions that are contained
;; in my .emacs and .emacs.d/ directories - e.g. path.join
;;

(require 'dizzee)


(dz-defservice adder "/home/david/emacs/site-packages/dizzee/test/services/adder.py")

(dz-register-reload adder "dizzee")

(adder-start)

(adder-stop)

(adder-start)

(adder-deregister-reload)
