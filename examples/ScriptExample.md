# Example Console Demo Setup

I received some messages indicating confusion as to how to set up and run the console demo in `ScriptExample.elm`,
so I decided to create and share this log to show the entire setup process.

I created a fresh Ubuntu virtual machine to show how to set up the console demo using the Elm IO library.
Before this log began, I installed haskell-platform, git, Elm, elm-get, nodejs, and npm. Other than this it was a completely
new, default Ubuntu installation. This log contains the complete process from this point:

```bash
deadfoxygrandpa@ubuntu:~$ git clone git://github.com/maxsnew/IO
Cloning into 'IO'...
remote: Reusing existing pack: 110, done.
remote: Counting objects: 4, done.
remote: Compressing objects: 100% (4/4), done.
remote: Total 114 (delta 0), reused 0 (delta 0)
Receiving objects: 100% (114/114), 64.69 KiB | 83.00 KiB/s, done.
Resolving deltas: 100% (52/52), done.
Checking connectivity... done
deadfoxygrandpa@ubuntu:~$ cd IO
deadfoxygrandpa@ubuntu:~/IO$ git checkout generalize
Branch generalize set up to track remote branch generalize from origin.
Switched to a new branch 'generalize'
deadfoxygrandpa@ubuntu:~/IO$ cabal install
Resolving dependencies...
Configuring ElmIO-0.1.0.0...
Building ElmIO-0.1.0.0...
Preprocessing executable 'elm-io' for ElmIO-0.1.0.0...
[1 of 2] Compiling Paths_ElmIO      ( dist/build/autogen/Paths_ElmIO.hs, dist/build/elm-io/elm-io-tmp/Paths_ElmIO.o )
[2 of 2] Compiling Main             ( mkExe.hs, dist/build/elm-io/elm-io-tmp/Main.o )
Linking dist/build/elm-io/elm-io ...
Installing executable(s) in /home/alex/.cabal/bin
Installed ElmIO-0.1.0.0
deadfoxygrandpa@ubuntu:~/IO$ sudo ln -s ~/.cabal/bin/elm-io /usr/local/bin/elm-io
[sudo] password for alex: 
deadfoxygrandpa@ubuntu:~/IO$ cd ..
deadfoxygrandpa@ubuntu:~$ git clone git://github.com/deadfoxygrandpa/Elm-Test
Cloning into 'Elm-Test'...
remote: Counting objects: 165, done.
remote: Compressing objects: 100% (103/103), done.
remote: Total 165 (delta 73), reused 137 (delta 58)
Receiving objects: 100% (165/165), 75.78 KiB | 53.00 KiB/s, done.
Resolving deltas: 100% (73/73), done.
Checking connectivity... done
deadfoxygrandpa@ubuntu:~$ cd Elm-Test/
deadfoxygrandpa@ubuntu:~/Elm-Test$ elm-get install evancz/automaton
Cloning repo evancz/automaton
Checking out version 0.1.0.1
Should I add this library to your elm_dependencies.json file? (y/n): y
Success!
deadfoxygrandpa@ubuntu:~/Elm-Test$ npm install jsdom
npm http GET http://registry.npmjs.org/jsdom
npm http 304 http://registry.npmjs.org/jsdom
npm http GET http://registry.npmjs.org/nwmatcher
npm http GET http://registry.npmjs.org/xmlhttprequest
npm http GET http://registry.npmjs.org/request
npm http GET http://registry.npmjs.org/cssom
npm http GET http://registry.npmjs.org/cssstyle
npm http GET http://registry.npmjs.org/htmlparser2
npm http GET http://registry.npmjs.org/contextify
npm http 304 http://registry.npmjs.org/xmlhttprequest
npm http 304 http://registry.npmjs.org/nwmatcher
npm WARN package.json nwmatcher@1.3.1 No README.md file found!
npm http 304 http://registry.npmjs.org/cssom
npm http 304 http://registry.npmjs.org/request
npm http 304 http://registry.npmjs.org/cssstyle
npm http 304 http://registry.npmjs.org/htmlparser2
npm http 304 http://registry.npmjs.org/contextify
npm http GET http://registry.npmjs.org/bindings
npm http GET http://registry.npmjs.org/qs
npm http GET http://registry.npmjs.org/json-stringify-safe
npm http GET http://registry.npmjs.org/tough-cookie
npm http GET http://registry.npmjs.org/hawk
npm http GET http://registry.npmjs.org/aws-sign2
npm http GET http://registry.npmjs.org/forever-agent
npm http GET http://registry.npmjs.org/node-uuid
npm http GET http://registry.npmjs.org/mime
npm http GET http://registry.npmjs.org/form-data
npm http GET http://registry.npmjs.org/tunnel-agent
npm http GET http://registry.npmjs.org/http-signature
npm http GET http://registry.npmjs.org/oauth-sign
npm http GET http://registry.npmjs.org/domhandler
npm http GET http://registry.npmjs.org/domutils
npm http GET http://registry.npmjs.org/domelementtype
npm http GET http://registry.npmjs.org/readable-stream
npm http 304 http://registry.npmjs.org/bindings
npm http GET http://registry.npmjs.org/bindings/-/bindings-1.1.1.tgz
npm http 200 http://registry.npmjs.org/json-stringify-safe
npm http GET http://registry.npmjs.org/json-stringify-safe/-/json-stringify-safe-5.0.0.tgz
npm http 304 http://registry.npmjs.org/forever-agent
npm http GET http://registry.npmjs.org/forever-agent/-/forever-agent-0.5.2.tgz
npm http 200 http://registry.npmjs.org/aws-sign2
npm http GET http://registry.npmjs.org/aws-sign2/-/aws-sign2-0.5.0.tgz
npm http 200 http://registry.npmjs.org/qs
npm http GET http://registry.npmjs.org/qs/-/qs-0.6.6.tgz
npm http 304 http://registry.npmjs.org/node-uuid
npm http GET http://registry.npmjs.org/node-uuid/-/node-uuid-1.4.1.tgz
npm http 304 http://registry.npmjs.org/mime
npm http GET http://registry.npmjs.org/mime/-/mime-1.2.11.tgz
npm http 304 http://registry.npmjs.org/form-data
npm http GET http://registry.npmjs.org/form-data/-/form-data-0.1.2.tgz
npm http 304 http://registry.npmjs.org/tunnel-agent
npm http GET http://registry.npmjs.org/tunnel-agent/-/tunnel-agent-0.3.0.tgz
npm http 304 http://registry.npmjs.org/http-signature
npm http GET http://registry.npmjs.org/http-signature/-/http-signature-0.10.0.tgz
npm http 304 http://registry.npmjs.org/oauth-sign
npm http GET http://registry.npmjs.org/oauth-sign/-/oauth-sign-0.3.0.tgz
npm http 200 http://registry.npmjs.org/tough-cookie
npm http GET http://registry.npmjs.org/tough-cookie/-/tough-cookie-0.12.1.tgz
npm http 200 http://registry.npmjs.org/domutils
npm http GET http://registry.npmjs.org/domutils/-/domutils-1.3.0.tgz
npm http 200 http://registry.npmjs.org/domelementtype
npm http GET http://registry.npmjs.org/domelementtype/-/domelementtype-1.1.1.tgz
npm http 200 http://registry.npmjs.org/bindings/-/bindings-1.1.1.tgz
npm http 200 http://registry.npmjs.org/readable-stream
npm http 200 http://registry.npmjs.org/json-stringify-safe/-/json-stringify-safe-5.0.0.tgz
npm http 200 http://registry.npmjs.org/domhandler
npm http GET http://registry.npmjs.org/readable-stream/-/readable-stream-1.1.11.tgz
npm http GET http://registry.npmjs.org/domhandler/-/domhandler-2.2.0.tgz
npm http 200 http://registry.npmjs.org/forever-agent/-/forever-agent-0.5.2.tgz
npm http 200 http://registry.npmjs.org/qs/-/qs-0.6.6.tgz
npm http 200 http://registry.npmjs.org/aws-sign2/-/aws-sign2-0.5.0.tgz
npm http 200 http://registry.npmjs.org/node-uuid/-/node-uuid-1.4.1.tgz
npm http 200 http://registry.npmjs.org/mime/-/mime-1.2.11.tgz
npm http 200 http://registry.npmjs.org/form-data/-/form-data-0.1.2.tgz
npm http 200 http://registry.npmjs.org/tunnel-agent/-/tunnel-agent-0.3.0.tgz

> contextify@0.1.6 install /home/alex/Programming/Elm-Test/node_modules/jsdom/node_modules/contextify
> node-gyp rebuild

npm http 200 http://registry.npmjs.org/http-signature/-/http-signature-0.10.0.tgz
npm http 200 http://registry.npmjs.org/oauth-sign/-/oauth-sign-0.3.0.tgz
npm http 200 http://registry.npmjs.org/tough-cookie/-/tough-cookie-0.12.1.tgz
npm http 200 http://registry.npmjs.org/domelementtype/-/domelementtype-1.1.1.tgz
npm http 200 http://registry.npmjs.org/readable-stream/-/readable-stream-1.1.11.tgz
npm http 200 http://registry.npmjs.org/domutils/-/domutils-1.3.0.tgz
npm http 200 http://registry.npmjs.org/domhandler/-/domhandler-2.2.0.tgz
make: Entering directory `/home/alex/Programming/Elm-Test/node_modules/jsdom/node_modules/contextify/build'
  CXX(target) Release/obj.target/contextify/src/contextify.o
npm http 200 http://registry.npmjs.org/hawk
npm http GET http://registry.npmjs.org/hawk/-/hawk-1.0.0.tgz
npm http 200 http://registry.npmjs.org/hawk/-/hawk-1.0.0.tgz
  SOLINK_MODULE(target) Release/obj.target/contextify.node
  SOLINK_MODULE(target) Release/obj.target/contextify.node: Finished
  COPY Release/contextify.node
make: Leaving directory `/home/alex/Programming/Elm-Test/node_modules/jsdom/node_modules/contextify/build'
npm http GET http://registry.npmjs.org/core-util-is
npm http GET http://registry.npmjs.org/string_decoder
npm http GET http://registry.npmjs.org/debuglog/0.0.2
npm http GET http://registry.npmjs.org/combined-stream
npm http GET http://registry.npmjs.org/async
npm http GET http://registry.npmjs.org/ctype/0.5.2
npm http GET http://registry.npmjs.org/assert-plus/0.1.2
npm http GET http://registry.npmjs.org/asn1/0.1.11
npm http 200 http://registry.npmjs.org/core-util-is
npm http 200 http://registry.npmjs.org/string_decoder
npm http GET http://registry.npmjs.org/core-util-is/-/core-util-is-1.0.1.tgz
npm http GET http://registry.npmjs.org/string_decoder/-/string_decoder-0.10.25-1.tgz
npm http 200 http://registry.npmjs.org/debuglog/0.0.2
npm http GET http://registry.npmjs.org/debuglog/-/debuglog-0.0.2.tgz
npm http GET http://registry.npmjs.org/punycode
npm http GET http://registry.npmjs.org/hoek
npm http GET http://registry.npmjs.org/boom
npm http GET http://registry.npmjs.org/cryptiles
npm http GET http://registry.npmjs.org/sntp
npm http 200 http://registry.npmjs.org/ctype/0.5.2
npm http GET http://registry.npmjs.org/ctype/-/ctype-0.5.2.tgz
npm http 200 http://registry.npmjs.org/assert-plus/0.1.2
npm http GET http://registry.npmjs.org/assert-plus/-/assert-plus-0.1.2.tgz
npm http 200 http://registry.npmjs.org/asn1/0.1.11
npm http GET http://registry.npmjs.org/asn1/-/asn1-0.1.11.tgz
npm http 200 http://registry.npmjs.org/combined-stream
npm http GET http://registry.npmjs.org/combined-stream/-/combined-stream-0.0.4.tgz
npm http 200 http://registry.npmjs.org/core-util-is/-/core-util-is-1.0.1.tgz
npm http 200 http://registry.npmjs.org/string_decoder/-/string_decoder-0.10.25-1.tgz
npm http 200 http://registry.npmjs.org/debuglog/-/debuglog-0.0.2.tgz
npm http 200 http://registry.npmjs.org/boom
npm http GET http://registry.npmjs.org/boom/-/boom-0.4.2.tgz
npm http 200 http://registry.npmjs.org/cryptiles
npm http GET http://registry.npmjs.org/cryptiles/-/cryptiles-0.2.2.tgz
npm http 200 http://registry.npmjs.org/async
npm http GET http://registry.npmjs.org/async/-/async-0.2.10.tgz
npm http 200 http://registry.npmjs.org/punycode
npm http GET http://registry.npmjs.org/punycode/-/punycode-1.2.4.tgz
npm http 200 http://registry.npmjs.org/ctype/-/ctype-0.5.2.tgz
npm http 200 http://registry.npmjs.org/assert-plus/-/assert-plus-0.1.2.tgz
npm http 200 http://registry.npmjs.org/sntp
npm http GET http://registry.npmjs.org/sntp/-/sntp-0.2.4.tgz
npm http 200 http://registry.npmjs.org/combined-stream/-/combined-stream-0.0.4.tgz
npm http 200 http://registry.npmjs.org/hoek
npm http 200 http://registry.npmjs.org/boom/-/boom-0.4.2.tgz
npm http GET http://registry.npmjs.org/hoek/-/hoek-0.9.1.tgz
npm http 200 http://registry.npmjs.org/asn1/-/asn1-0.1.11.tgz
npm http 200 http://registry.npmjs.org/cryptiles/-/cryptiles-0.2.2.tgz
npm http 200 http://registry.npmjs.org/async/-/async-0.2.10.tgz
npm http 200 http://registry.npmjs.org/punycode/-/punycode-1.2.4.tgz
npm http 200 http://registry.npmjs.org/sntp/-/sntp-0.2.4.tgz
npm http 200 http://registry.npmjs.org/hoek/-/hoek-0.9.1.tgz
npm http GET http://registry.npmjs.org/delayed-stream/0.0.5
npm http 200 http://registry.npmjs.org/delayed-stream/0.0.5
npm http GET http://registry.npmjs.org/delayed-stream/-/delayed-stream-0.0.5.tgz
npm http 200 http://registry.npmjs.org/delayed-stream/-/delayed-stream-0.0.5.tgz
jsdom@0.10.1 node_modules/jsdom
├── xmlhttprequest@1.6.0
├── cssom@0.3.0
├── nwmatcher@1.3.1
├── cssstyle@0.2.9
├── contextify@0.1.6 (bindings@1.1.1)
├── htmlparser2@3.5.1 (domelementtype@1.1.1, domutils@1.3.0, domhandler@2.2.0, readable-stream@1.1.11)
└── request@2.34.0 (json-stringify-safe@5.0.0, forever-agent@0.5.2, aws-sign2@0.5.0, qs@0.6.6, tunnel-agent@0.3.0, oauth-sign@0.3.0, node-uuid@1.4.1, mime@1.2.11, tough-cookie@0.12.1, hawk@1.0.0, http-signature@0.10.0, form-data@0.1.2)
deadfoxygrandpa@ubuntu:~/Elm-Test$ elm-io ScriptExample.elm ScriptExample.js
[1 of 10] Compiling IO.IO               ( /home/alex/.cabal/share/ElmIO-0.1.0.0/IO/IO.elm )
[2 of 10] Compiling ElmTest.Assertion   ( ElmTest/Assertion.elm )
[3 of 10] Compiling ElmTest.Test        ( ElmTest/Test.elm )
[4 of 10] Compiling ElmTest.Run         ( ElmTest/Run.elm )
[5 of 10] Compiling ElmTest.Runner.String ( ElmTest/Runner/String.elm )
[6 of 10] Compiling ElmTest.Runner.Console ( ElmTest/Runner/Console.elm )
[7 of 10] Compiling Test                ( Test.elm )
[8 of 10] Compiling Automaton           ( elm_dependencies/evancz-automaton/0.1.0.1/Automaton.elm )
[9 of 10] Compiling IO.Runner           ( /home/alex/.cabal/share/ElmIO-0.1.0.0/IO/Runner.elm )
[10 of 10] Compiling Main                ( ScriptExample.elm )
Generating JavaScript ... Done
Making exe
deadfoxygrandpa@ubuntu:~/Elm-Test$ nodejs ScriptExample.js
  4 tests executed
  3 tests passed
  1 tests failed
8 == 1: FAILED. Expected: 8; got: 1
3 == 3: passed.
True: passed.
test head: passed.
deadfoxygrandpa@ubuntu:~/Elm-Test$ 
```

I hope this clears up any confusion!
