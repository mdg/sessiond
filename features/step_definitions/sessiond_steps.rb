require 'rest-client'
require 'json'
require 'amqp'
require 'test/unit/assertions'
World(Test::Unit::Assertions)


When /^I create a session for (.*)$/ do |userid|
	actual_response = RestClient.post('http://localhost:8443/create',
				   :userid => userid)
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


Then /^response should be (.*)$/ do |expected_response|
	actual_response = instance_variable_get("@actual_response")

	assert_equal(expected_response, actual_response)
end
