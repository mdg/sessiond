

file 'sessiond.beam' => ['sessiond.erl'] do |t|
	sh('erlc sessiond.erl')
end

task :app => ['sessiond.beam']

task :default => ['app']

task :devrun => ['app'] do |t|
	sh('erl -pa mochiweb/deps/mochiweb-src/ebin/ -s reloader -s sessiond start')
end

task :run => ['app'] do |t|
	sh('erl -noshell -s sessiond start -s init stop')
end

