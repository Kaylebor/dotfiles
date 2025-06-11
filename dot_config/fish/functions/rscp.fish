function rscp --wraps='rsync -HAXhaJxP --stats' --description 'alias rscp rsync -HAXhaJxP --stats'
  rsync -HAXhaJxP --stats $argv
        
end
