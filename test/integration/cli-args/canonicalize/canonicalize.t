Cmdliner error output can be different if color is enabled
  $ export NO_COLOR=1
Test that a nonsense argument is caught
  $ stanc --canonicalize dummy
  Usage: stanc [--help] [OPTION]… [MODEL_FILE]
  stanc: option '--canonicalize': invalid element in list ('dummy'): invalid
         value 'dummy', expected one of 'deprecations', 'parentheses',
         'braces', 'includes' or 'strip-comments'
  [124]

Test capitalization - this should fail due to the lack of model_name, not the canonicalizer
  $ stanc --canonicalize DEPRECATIONS,parentheses,bRaCeS
  Usage: stanc [--help] [OPTION]… [MODEL_FILE]
  stanc: No model file provided
  [124]
