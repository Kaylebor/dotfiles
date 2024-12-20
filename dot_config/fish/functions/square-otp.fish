function square-otp --wraps='curl -s http://127.0.0.1:4646/ffxivlauncher/(op item get "Square Enix ID" --otp) > /dev/null' --description 'alias square-otp curl -s http://127.0.0.1:4646/ffxivlauncher/(op item get "Square Enix ID" --otp) > /dev/null'
  curl -s http://127.0.0.1:4646/ffxivlauncher/(op item get "Square Enix ID" --otp) > /dev/null $argv
        
end
