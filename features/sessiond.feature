Feature: Sessiond
  Store and retrieve live sessions

  Scenario Outline: Create
    When I create a session for <User>
    Then sessionid should be <SessionID> in response
    When I check session for <User>
    Then absolute_exp should be part of the response

    Examples:
      | User      | SessionID     |
      | dog       | sessiondog    |

  Scenario Outline: Create and Kill
    When I create a session for <User>
    When I kill session for <User>
    Then killed should be <Killed> in response

    Examples:
      | User      | Killed  |
      | dog       | true    |

  Scenario Outline: Just Kill
    When I kill session for <User>
    Then killed should be <Killed> in response

    Examples:
      | User      | Killed  |
      | horse     | false   |

  Scenario Outline: Create and Renew
    When I create a session for <User>
    When I renew session for <User>
    Then live should be <Live> in response

    Examples:
      | User      | Live |
      | dog       | true |

  Scenario Outline: Create, Kill and Renew
    When I create a session for <User>
    When I kill session for <User>
    When I renew session for <User>
    Then live should be <Live> in response

    Examples:
      | User      | Live    |
      | dog       | false   |

  Scenario Outline: Just Renew
    When I renew session for <User>
    Then live should be <Live> in response

    Examples:
      | User      | Live    |
      | dog       | false   |

  Scenario Outline: Create and Queue Renew
    When I create a session for <User>
    When I sleep for <Sleep> seconds
    When I queue renew session for <User>
    When I check session for <User>
    Then live should be <Live> in response

    Examples:
      | User    | Sleep | Live  |
      | dogs    | 1     | true  |

  Scenario Outline: Just Queue Renew
    When I queue renew session for <User>
    When I check session for <User>
    Then live should be <Live> in response

    Examples:
      | User      | Live    |
      | mouse     | false   |
