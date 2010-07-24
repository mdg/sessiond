Feature: Sessiond
  Store and retrieve live sessions

  Scenario Outline: Series
    When I create a session for <UserID>
    Then it should create session <SessionID>

    Examples:
      | UserID    | SessionID     |
      | dog       | sessiondog    |
      | cat       | sessioncat    |
