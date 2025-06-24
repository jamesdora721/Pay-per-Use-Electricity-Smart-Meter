import { assertEquals, runScenario } from '@stacks/clarity-js-sdk'; import { describe, it } from '@types/mocha';
describe('Pay-per-Use Electricity Smart Meter', () => {
  const accounts = {
    deployer: "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM",
    user1: "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG",
    user2: "ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC"
  };

  describe('initialization', () => {
    it('should initialize meter successfully', () => {
      runScenario(async (chain: any) => {
        const result = chain.callPublic('initialize-meter', [], accounts.user1);
        assertEquals(result.success, true);
      });
    });
  });

  describe('top-up', () => {
    it('should allow valid top-up', () => {
      runScenario(async (chain: any) => {
        chain.callPublic('initialize-meter', [], accounts.user1);
        const result = chain.callPublic('top-up', [1500], accounts.user1);
        assertEquals(result.success, true);
      });
    });

    it('should reject invalid amount', () => {
      runScenario(async (chain: any) => {
        chain.callPublic('initialize-meter', [], accounts.user1);
        const result = chain.callPublic('top-up', [500], accounts.user1);
        assertEquals(result.success, false);
      });
    });
  });

  describe('consume-units', () => {
    it('should allow valid consumption', () => {
      runScenario(async (chain: any) => {
        chain.callPublic('initialize-meter', [], accounts.user1);
        chain.callPublic('top-up', [2000], accounts.user1);
        const result = chain.callPublic('consume-units', [10], accounts.user1);
        assertEquals(result.success, true);
      });
    });
  });
});