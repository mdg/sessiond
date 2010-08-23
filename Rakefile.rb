

file 'ebin/sessiond.beam' => ['src/sessiond.erl'] do |t|
	sh('erlc -o ebin/ src/sessiond.erl')
end

task :app => ['ebin/sessiond.beam']

task :default => ['app']

task :devrun => ['app'] do |t|
	sh('erl -pa ebin deps/mochiweb deps/rabbitmq-erlang-client deps/rabbitmq-server -s sessiond start')
end

task :run => ['app'] do |t|
	sh('erl -pa ebin deps/mochiweb deps/rabbitmq-erlang-client deps/rabbitmq-server -noshell -s sessiond start')
end

