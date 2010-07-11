

task 'sessiond.beam' => ['sessiond.erl'] do |t|
	sh('erlc sessiond.erl')
end

task :app => ['sessiond.beam']

task :default => ['app']

task :run => ['app'] do |t|
	sh('erl -noshell -s sessiond start -s init stop')
end

