# Simple Release note generator
 This application will generate release note for mentor project. 
 You need to have GHC platform installed to run this project.
 
## Steps to run
    
  1. Rename the config.cfg.template file to config.cfg
  
  2. Update the config.cfg file for correct username and password.
  
  3. Run `cabal sandbox init`. You only need to run this first time. This will initialize a sandbox in the project dirctory so that your global dependencies stay clean.
  
  4. Run `cabal install --only-dependencies`. This may take some time.
  
  5. Run `cabal build`
  
  6. Run `./dist/build/notegen/notegen <TAG-FROM> <TAG-TO>`
