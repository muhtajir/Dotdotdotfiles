function __fish_load_event_handlers
    set -l handler_list __fish_bell_on_completion_handle

    for handler in $handler_list
        source $fish_function_path_local/{$handler}.fish
    end
end
