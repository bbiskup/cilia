#!/bin/bash -e                                                                                                                                                        
                                                                                 
 # Build Docker container for development                                       
                                                                                
 ln -sf $PWD/git-hooks/pre-commit .git/hooks/                                   
 touch .bash_history                                                            
 docker-compose up -d 
 docker-compose build                                                           