require 'rest-client'
require 'json'
require 'test/unit/assertions'
World(Test::Unit::Assertions)


When /^I create a session for (.*)$/ do |userid|
	response = RestClient.post('http://localhost:8443/create',
				   :userid => userid)
	instance_variable_set("@userid", userid)
	instance_variable_set("@response", response)
end

Then /^it should create (.*)$/ do |sessionid|
	#assert_equal('a', 'hello world')
	userid = instance_variable_get("@userid")
	response = instance_variable_get("@response")

	expected = "{\"sessionid\":\"session#{userid}\"}"
	assert_equal(expected, response)
end

When /^I renew session (.*)$/ do |sessionid|
	response = RestClient.post("http://localhost:8443/renew",
				   :sessionid => sessionid)
	instance_variable_set("@sessionid", sessionid)
	instance_variable_set("@response", response)
end

Then /^it should renew (.*)$/ do |sessionid|
end
