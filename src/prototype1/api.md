
# sample datatype #                    # joint inference API #
Data:                                  For building distribution:
  - val                                  - (gaussian 0 1)
  - forced?                                  --> registers and returns new sample
  - handled?                             - generic overloads (e.g. + and *)

e.g.:                                  For running specialized algorithms:
  - (sample:force sample)                - (construct-joint-sample)
     --> -1.87                           - (marginal-likelihood)
  - (sample:force-set! sample 2.2)
     --> conditioning                  For the MH process:
                                       - (roll-back-to epoch)
                                       - (getstate)
                                       - (setstate foo)












