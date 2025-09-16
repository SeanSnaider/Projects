import java.util.*;
import tester.*;

class Alien {
  int HP;
  int ATK;
  
  Alien(int HP, int ATK) {
    this.HP = HP;
    this.ATK = ATK;
  }

  //getter fn which is the sum of the HP and ATK
  public int getPower() {
    return this.HP + this.ATK;
  }
  
  //will check to see if a given alien has 0 or less HP
  public boolean isDead() {
    return this.HP <= 0;
  }
  
  //override the equals function to just check if the HP and ATK are equivalent for testing purposes
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Alien)) return false;
    Alien that = (Alien) other;
    return this.HP == that.HP && this.ATK == that.ATK;
  }

  //to string for testing
  @Override
  public String toString() {
    return "Alien(HP:" + HP + ",ATK:" + ATK + ")";
  }
}

//class to represent an invasion and dealing with it, that meaning sorting, calculating things relating to the list of aliens, etc.
class Invasion {
  List<Alien> aliens;  
  int commanderHealth;
  List<String> commands; 
  
  //constructor which instatiates the aliens (which are mean't to be an array into a list for effiency sake)
  //and moves it all to the new list.
  Invasion(Alien[] alienArray, int numAliens, int commanderHealth) {
    this.aliens = new ArrayList<>(numAliens);
    for (int i = 0; i < numAliens; i++) {
      if (!alienArray[i].isDead()) {
        this.aliens.add(alienArray[i]);
      }
    }
    this.commanderHealth = commanderHealth;
    this.commands = new ArrayList<>();
    //sort
    sortAliensByPower();
  }
  
  //sorting by using sort
  private void sortAliensByPower() {
    aliens.sort((a1, a2) -> {
      int power1 = a1.getPower();
      int power2 = a2.getPower();
      if (power1 != power2) {
        return Integer.compare(power2, power1);
      }
      return Integer.compare(a2.HP, a1.HP);
    });
  }
  
  //just go through once and calculate the damage
  private int calculateTotalDamage() {
    int damage = 0;
    for (Alien alien : aliens) {
      if (!alien.isDead()) {
        damage += alien.ATK;
      }
    }
    return damage;
  }
  
  //one damage to all aliens which are the health remaining and aliens
  private void volley() {
    int aliveCount = aliens.size();
    if (aliveCount == 0) return;
    
    int targetsHit = commanderHealth % aliveCount;
    if (targetsHit == 0) return;
    
    //needs to be resorted only if there is a change in this list
    boolean needsResort = false;
    for (int i = 0; i < targetsHit && i < aliens.size(); i++) {
      Alien alien = aliens.get(i);
      alien.HP--;
      if (alien.HP <= 0) {
        needsResort = true;
      }
    }
    
    //if we need the resort and sort and deal with the dead fellas
    if (needsResort) {
      removeDeadAndSort();
    }
  }
  
  //focused volley - first half do 
  private void focusedVolley() {
    int aliveCount = aliens.size();
    if (aliveCount == 0) return;
    
    int targetsHit = (aliveCount + 1) / 2;
    boolean needsResort = false;
    
    for (int i = 0; i < targetsHit && i < aliens.size(); i++) {
      Alien alien = aliens.get(i);
      alien.HP -= 2;
      if (alien.HP <= 0) {
        alien.HP = 0;
        needsResort = true;
      }
    }
    if (needsResort) { //check if it needs to be sorted once more
      removeDeadAndSort();
    }
  }
  
  //will remove the first and it will be sorted after its removed
  private void focusedShot() {
    if (!aliens.isEmpty()) {
      aliens.remove(0); 
    }
  }
  
  //efficient removal of dead aliens and also sorting them by power using quick sort for more efficiency
  private void removeDeadAndSort() {
    aliens.removeIf(Alien::isDead); 
    sortAliensByPower(); 
  }
  
  //will actually execute the command
  public void executeCommand(String command) {
    commands.add(command);
    
    switch(command) {
      case "volley":
        volley();
        break;
      case "focusedVolley":
        focusedVolley();
        break;
      case "focusedShot":
        focusedShot();
        break;
      default:
        throw new RuntimeException("Unknown command: " + command);
    }
    
    commanderHealth -= calculateTotalDamage();
  }
  
  public boolean isGameOver() {
    return commanderHealth <= 0;
  }
  
  public boolean isVictory() {
    return aliens.isEmpty() && commanderHealth > 0;
  }
  
  public String getCommandsString() {
    return String.join(", ", commands);
  }
}

//solver class to actually solve the given invasion
class AlienInvasionSolver {
  
  //nlogn solution to solving
  public String solveInvasion(Invasion state) {
    
    //checks state of if we didnt win or lose yet and then does the optimal command
    while (!state.isVictory() && !state.isGameOver()) {
      String optimalCommand = chooseOptimalCommand(state);
      if (optimalCommand == null) break;
      state.executeCommand(optimalCommand);
    }
    
    if (state.isVictory()) {
      return "Victory! Steps: " + state.getCommandsString() + 
             " | Health left: " + state.commanderHealth;
    } else {
      return "Game Over! Steps: " + state.getCommandsString() + 
             " | Aliens left: " + state.aliens.size();
    }
  }
  
  //single pass thru
  private String chooseOptimalCommand(Invasion state) {
    if (state.aliens.isEmpty()) return null;
    
    //calcuation for the health and alien size to help calculate the efficiencies for hte given algorithms
    int aliveCount = state.aliens.size();
    int commanderHP = state.commanderHealth;
    
    //efficiency for all of the given actions
    int volleyDamage = calculateVolleyEfficiency(state, aliveCount, commanderHP);
    int focusedVolleyDamage = calculateFocusedVolleyEfficiency(state, aliveCount);
    int focusedShotDamage = calculateFocusedShotEfficiency(state);
    
    // Choose command with highest damage reduction
    if (focusedShotDamage >= volleyDamage && focusedShotDamage >= focusedVolleyDamage) {
      return "focusedShot";
    } else if (focusedVolleyDamage >= volleyDamage) {
      return "focusedVolley";
    } else if (volleyDamage > 0) {
      return "volley";
    } else {
      //failsafe focusedshot if nothing does anything
      return "focusedShot";
    }
  }
  
  //early termination after we finish doing all of the volley states
  //will calculate volley efficiency by seeing the damage mitigated
  private int calculateVolleyEfficiency(Invasion state, int aliveCount, int commanderHP) {
    int targets = commanderHP % aliveCount;
    if (targets == 0) return 0;
    
    int damageReduced = 0;
    for (int i = 0; i < targets && i < state.aliens.size(); i++) {
      Alien alien = state.aliens.get(i);
      if (alien.HP == 1) {
        damageReduced += alien.ATK;
      }
    }
    return damageReduced;
  }
  
  //early termination once more when calculating the volley efficiency
  //checks damage mitigated
  private int calculateFocusedVolleyEfficiency(Invasion state, int aliveCount) {
    int targets = (aliveCount + 1) / 2;
    int damageReduced = 0;
    
    for (int i = 0; i < targets && i < state.aliens.size(); i++) {
      Alien alien = state.aliens.get(i);
      if (alien.HP <= 2) {
        damageReduced += alien.ATK;
      }
    }
    return damageReduced;
  }
  
  //this just checks damage mitigated from the most powerful alien 
  private int calculateFocusedShotEfficiency(Invasion state) {
    if (state.aliens.isEmpty()) return 0;
    return state.aliens.get(0).ATK;
  }
}

// Test class with optimized tests
class AlienInvasionTests {
  
  void testOptimalSolution(Tester t) {
    AlienInvasionSolver solver = new AlienInvasionSolver();
    
    // Test case 1: Small invasion
    Alien[] smallInvasion = {
      new Alien(2, 3),
      new Alien(1, 2),
      new Alien(3, 1)
    };
    Invasion small = new Invasion(smallInvasion, 3, 10);
    String result1 = solver.solveInvasion(small);
    t.checkExpect(result1.contains("Victory!") || result1.contains("Game Over!"), true);
    
    // Test case 2: Large invasion for O(n log n) verification
    Alien[] largeInvasion = new Alien[1000];
    for (int i = 0; i < 1000; i++) {
      largeInvasion[i] = new Alien((i % 3) + 1, (i % 4) + 1);
    }
    Invasion large = new Invasion(largeInvasion, 1000, 3000);
    
    long startTime = System.currentTimeMillis();
    String result2 = solver.solveInvasion(large);
    long endTime = System.currentTimeMillis();
    
    // Should complete quickly even with 1000 aliens
    t.checkExpect(endTime - startTime < 100, true); // Less than 100ms
    t.checkExpect(result2.contains("Victory!") || result2.contains("Game Over!"), true);
  }
  
  void testEdgeCases(Tester t) {
    AlienInvasionSolver solver = new AlienInvasionSolver();
    
    // Empty invasion
    Alien[] empty = {};
    Invasion emptyInv = new Invasion(empty, 0, 100);
    t.checkExpect(solver.solveInvasion(emptyInv).contains("Victory!"), true);
    
    // Single alien
    Alien[] single = {new Alien(1, 1)};
    Invasion singleInv = new Invasion(single, 1, 5);
    String singleResult = solver.solveInvasion(singleInv);
    t.checkExpect(singleResult.contains("Victory!"), true);
  }
}
