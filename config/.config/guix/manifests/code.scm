;; This began as a manifest for a .guix-extra-profiles "code" profile
;; for installing mostly python and julia packages for ml

(specifications->manifest
 '(;; C/C++
   "gcc-toolchain"
   "make"

   ;; Python
   "python"
   ))
