function __ssh_agent_start -d "start a new ssh agent"
    ssh-agent -c | string replace -r '^echo' '# echo' > $SSH_ENV
    chmod 600 $SSH_ENV
    source $SSH_ENV > /dev/null
end
