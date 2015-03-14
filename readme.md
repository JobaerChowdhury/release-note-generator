# Simple Release note generator
 This application will generate release note for mentor project. 
 You need to have GHC platform installed to run this project.
 
## Steps to run
    
  1. Rename the config.cfg.template file to config.cfg
  
  2. Update the config.cfg file for correct username and password.
  
  3. Run `cabal build`
  
  4. Run `./dist/build/notegen/notegen <TAG-FROM> <TAG-TO>`