Feature: Sessiond
  Store and retrieve live sessions

  Scenario Outline: Create
    When I create a session for <UserID>
    Then response should be <Response>

    Examples:
      | UserID    | Response                      |
      | dog       | {"sessionid":"sessiondog"}    |
      | cat       | {"sessionid":"sessioncat"}    |

  Scenario Outline: Create and Kill
    When I create a session for <UserID>
    When I kill session for <UserID>
    Then response should be <Response>

    Examples:
      | UserID    | Response           |
      | dog       | {"killed":true}    |

  Scenario Outline: Just Kill
    When I kill session for <UserID>
    Then response should be <Response>

    Examples:
      | UserID    | Response           |
      | horse     | {"killed":false}   |

  Scenario Outline: Create and Renew
    When I create a session for <UserID>
    When I renew session for <UserID>
    Then response should be <Response>

    Examples:
      | UserID    | Response                        |
      | dog       | {"live":true,"userid":"dog"}    |

  Scenario Outline: Create, Kill and Renew
    When I create a session for <UserID>
    When I kill session for <UserID>
    When I renew session for <UserID>
    Then response should be <Response>

    Examples:
      | UserID    | Response         |
      | dog       | {"live":false}   |

  Scenario Outline: Just Renew
    When I renew session for <UserID>
    Then response should be <Response>

    Examples:
      | UserID    | Response         |
      | dog       | {"live":false}   |
