FROM lambda-terminal-base
ENV TERM=xterm-256color
VOLUME /codebase
ENTRYPOINT lambda-terminal /codebase/
