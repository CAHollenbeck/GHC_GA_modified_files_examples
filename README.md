# Modified files for my thesis work (example code by request)
These are some of the files modified to produce training data for the genetic algorithm and neural networks used in my thesis.

All changes to GHC are marked with comments which say "uoe"

## Files with significant changes include:

### ByteCodeInstr.hs
Contains a function `funcIdxs` which parses a function body and gets integer representations of its syntax nodes by finding their value in the `FunInlineFeature` enum data type. Helper functions are present and documented.

### CoreUnfold.hs
Added functionality to calculate expression size for some unfoldings. Added functions, `uoe___Size`, to calculate approximate expression sizes.

This is the file where feature vectors are printed from the inlining decisions in `tryUnfolding_trace` during compilation, and these vectors were used for training data for neural networks.

The function `callSiteInline` was modified to call to a neural network model passed in through the dynamic flags, where it passes the feature vectors to the model, receives a numerical value, and applies an activation threshold to decide inlining.

### DynFlags.hs
Added functionality to pass in a neural network via dynamic flags. The NN was a JSON representation of nodes, weights, and activation information.

### GAUtils.hs
File I introduced to add functionality to decode and activate the NN passed in via dynamic flags.

### Simplify.hs
Functionality to get feature vector information for the unfolding's arguments. Other small modifications.
