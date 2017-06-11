function fish_vi_cursor --description 'Set cursor shape for different vi modes'
	function fish_vi_cursor_handle --on-variable fish_bind_mode --on-event fish_postexec
          if test $fish_bind_mode = "insert"
              echo -ne '\e[0 q'
          else
              echo -ne '\e[1 q'
          end
      end

      function fish_vi_cursor_handle_preexec --on-event fish_preexec
          if test $fish_bind_mode = "insert"
              echo -ne '\e[0 q'
          else
              echo -ne '\e[1 q'
          end
      end
end
