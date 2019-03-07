module CustomerService.Acceptance.UpdateCustomerSpec where

import Test.Hspec

{--
Feature: update

  Scenario: update
    Given customer "Nicole Forsgren" with ID "12345" exists
    When update customer "12345" with "Nicole X"
    Then customer "12345" should be "Nicole X"
--}

spec :: Spec
spec = describe "Feature: Update customer" $
  it "does nothing yet" pending