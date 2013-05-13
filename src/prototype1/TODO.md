# definitely #
* undoable effects
* save the number of accepts and rejects

# maybe #
* make pramb a macro like amb so it doesn't evaluate its arguments?

# next version #
* some lazy-evaluating magic for gaussians, conjugacy, etc

# notes #
* compiling with clang seemed to yield wrong answers, wtf! test pieces


# emit controller for smart stuff #
* call all handlers in registry, which mark samples as handled (and nothing else)
* cond
    - if everything is handled or loop counter is 0 => call all construct joint
      samples in registry and fall through
    - if not everything is handled but emitted thing is and we don't have a
      non-exact likelihood => call marg likelihood and try another (unhandled)
      choice (non-exact likelihood forces)
    - if emitted thing is not handled but has an explicit likelihood => call
      explicit likelihood as before
    - else error!

# NOTES #
* rollback should reset handled flags
* marg likelihood should 
