# definitely #
* undoable effects
* save the number of accepts and rejects


# emit controller for smart stuff #
* call all handlers in registry, which mark samples as handled (and nothing else)
* cond
    - if everything is handled or loop counter is 0 => call all construct joint
      samples in registry and fall through
    - if (not everything is handled but) emitted thing is handled and we have
      an exact likelihood => call marg likelihood and try another (unhandled)
      choice (non-exact likelihood forces)
    - ELSE mark all as unhandled and do full MH

    - what about when emitted thing is not handled (or forced by likelihood)
      but other things are? can we handle that, or is it unhandlable?

# NOTES #
* rollback should reset handled flags
* marg likelihood should unset the forced flag? 
