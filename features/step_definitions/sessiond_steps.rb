require 'rest-client'
require 'json'
require 'amqp'
require 'test/unit/assertions'
World(Test::Unit::Assertions)


When /^I create a session for ([a-zA-Z0-9]*)$/ do |userid|
	actual_response = RestClient.post('http://localhost:8443/create',
				   :userid => userid)
	instance_variable_set("@userid", userid)
	instance_variable_set("@actual_response", actual_response)
end

When /^I create a session for ([a-zA-Z0-9]*) with timeout ([0-9]*)$/ do |userid, timeout|
	timeout = Integer(timeout)
	actual_response = RestClient.post('http://localhost:8443/create',
				   :userid => userid, :timeout => timeout)
	instance_variable_set("@userid", userid)
	instance_variable_set("@actual_response", actual_response)
end

When /^I renew session for (.*)$/ do |userid|
	sessionid= "session#{userid}"
	response = RestClient.post("http://localhost:8443/renew",
				   :sessionid => sessionid)

	instance_variable_set("@actual_response", response)
end

When /^I kill session for (.*)$/ do |userid|
	sessionid= "session#{userid}"
	actual_response = RestClient.post("http://localhost:8443/kill",
				   :sessionid => sessionid)

	instance_variable_set("@userid", userid)
	instance_variable_set("@sessionid", sessionid)
	instance_variable_set("@actual_response", actual_response)
end

When /^I check session for (.*)$/ do |userid|
	sessionid= "session#{userid}"
	response = RestClient.post("http://localhost:8443/live",
				   :sessionid => sessionid)

	instance_variable_set("@userid", userid)
	instance_variable_set("@sessionid", sessionid)
	instance_variable_set("@actual_response", response)
end


When /^I queue renew session for (.*)$/ do |userid|
end


When /^I sleep for ([0-9]*) seconds$/ do |seconds|
	sleep(Integer(seconds))
end


Then /^response should be (.*)$/ do |expected_response|
	actual_response = instance_variable_get("@actual_response")

	assert_equal(expected_response, actual_response)
end

Then /^([a-zA-Z]*) should be ([a-zA-Z0-9]*) in response$/ do |key,expected_val|
	response = instance_variable_get("@actual_response")

	respobj = JSON.parse(response)
	actual_value = respobj[key]
	assert_equal(expected_val, actual_value.to_s)
end
