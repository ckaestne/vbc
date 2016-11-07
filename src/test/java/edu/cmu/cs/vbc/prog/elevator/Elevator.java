package edu.cmu.cs.vbc.prog.elevator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


public   class  Elevator {
	
	//@Symbolic("false")
	Environment env;

	
	//@Symbolic("false")
	boolean verbose;

	
	//@Symbolic("false")
	int currentFloorID;

	
	public enum  Direction {
		up { 
			@Override public Direction reverse() {return down;}	
		} ,  
		down { 
			@Override public Direction reverse() {return up;}
		}; 
		public abstract Direction reverse();}

	
	//@Symbolic("false")
	Direction currentHeading;

	
	//@Symbolic("false")
	private List<Person> persons = new ArrayList<Person>();


	 enum  DoorState {open ,  close}

	;


	//@Symbolic("false")
	DoorState doors;


	//@Symbolic("false")
	int[] floorButtons;



	public Elevator(Environment env, boolean verbose) {
		this.verbose = verbose;
		this.currentHeading = Direction.up;
		this.currentFloorID = 0;
		this.doors = DoorState.open;
		this.env = env;
		this.floorButtons = new int[env.floors.length];
	}


	public Elevator(Environment env, boolean verbose, int floor, boolean headingUp) {
		this.verbose = verbose;
		this.currentHeading = (headingUp ? Direction.up : Direction.down);
		this.currentFloorID = floor;
		this.doors = DoorState.open;
		this.env = env;
		this.floorButtons = new int[env.floors.length];
	}



	public boolean  isBlocked__before__overloaded() {
		return false;
	}



	public boolean  isBlocked__role__overloaded() {
		return blocked;
	}


	public boolean
isBlocked() {
    if (FeatureSwitches.__SELECTED_FEATURE_overloaded) {
        return isBlocked__role__overloaded();
    } else {
        return isBlocked__before__overloaded();
    }
}





	public void  enterElevator__before__weight(Person p) {
		persons.add(p);
		p.enterElevator(this);
		if (verbose) System.out.println(p.getName() + " entered the Elevator at Landing " + String.valueOf(this.getCurrentFloorID()) + ", going to " + String.valueOf(p.getDestination()));
	}



	public void  enterElevator__role__weight(Person p) {
		enterElevator__before__weight(p);
		weight+=p.getWeight();
	}


	public void
enterElevator(Person p) {
    if (FeatureSwitches.__SELECTED_FEATURE_weight) {
        enterElevator__role__weight(p);
    } else {
        enterElevator__before__weight(p);
    }
}





	public boolean  leaveElevator__before__weight(Person p) {
		if (persons.contains(p)) {
			persons.remove(p);
			p.leaveElevator();
			if (verbose) System.out.println(p.getName() + " left the Elevator at Landing " + String.valueOf(currentFloorID));
			return true;
		} else return false;
	}



	public boolean  leaveElevator__role__weight(Person p) {
		if (leaveElevator__before__weight(p)) {
			weight-=p.getWeight();
			return true;
		} else return false;
	}



	public boolean
leaveElevator__before__empty(Person p) {
    if (FeatureSwitches.__SELECTED_FEATURE_weight) {
        return leaveElevator__role__weight(p);
    } else {
        return leaveElevator__before__weight(p);
    }
}





	public boolean  leaveElevator__role__empty(Person p) { // empty
		if (leaveElevator__before__empty(p)) {
			if (this.persons.isEmpty())
				Arrays.fill(this.floorButtons, 0);
			return true;
		} else return false;
	}


	public boolean
leaveElevator(Person p) {
    if (FeatureSwitches.__SELECTED_FEATURE_empty) {
        return leaveElevator__role__empty(p);
    } else {
        return leaveElevator__before__empty(p);
    }
}





	/**
	 * Activates the button for the given floor in the lift.
	 * @param floorID
	 */
	public void pressInLiftFloorButton(int floorID) {
		floorButtons[floorID] = 1;
	}


	private void resetFloorButton(int floorID) {
		floorButtons[floorID] = 0;
	}


	public int getCurrentFloorID() {
		return currentFloorID;
	}



	public boolean areDoorsOpen() {
		return doors == DoorState.open;
	}


	// pre: elevator arrived at the current floor, next actions to be done
	private void  timeShift__before__overloaded() {
		//System.out.println("--");

		if (stopRequestedAtCurrentFloor()) {
			//System.out.println("Arriving at " +  currentFloorID + ", Doors opening");
			doors = DoorState.open;
			// iterate over a copy of the original list, avoids concurrent modification exception
			for (Person p: new ArrayList<Person>(persons)) {
				if (p.getDestination() == currentFloorID) {
					leaveElevator(p);
				}
			}
			env.getFloor(currentFloorID).processWaitingPersons(this);
			resetFloorButton(currentFloorID);
		} else {
			if (doors == DoorState.open)  {
				doors = DoorState.close;
				//System.out.println("Doors Closing");
			}
			if (stopRequestedInDirection(currentHeading, true, true)) {
				//System.out.println("Arriving at " + currentFloorID + ", continuing");
				// continue
				continueInDirection(currentHeading);
			} else if (stopRequestedInDirection(currentHeading.reverse(), true, true)) {
				//System.out.println("Arriving at " + currentFloorID + ", reversing direction because of call in other direction");
				// revert direction
				continueInDirection(currentHeading.reverse());
			} else {
				//idle
				//System.out.println("Arriving at " + currentFloorID + ", idle->continuing");
				continueInDirection(currentHeading);
			}
		}
	}


	// pre: elevator arrived at the current floor, next actions to be done
	private void  timeShift__role__overloaded() {
		if (areDoorsOpen() && weight > maximumWeight) {
			blocked = true;
			if (verbose) System.out.println("Elevator blocked due to overloading (weight:" + String.valueOf(weight) + " > maximumWeight:" + String.valueOf(maximumWeight) + ")");
		} else {
			blocked = false;
			timeShift__before__overloaded();
		}
	}


	public void
timeShift() {
    if (FeatureSwitches.__SELECTED_FEATURE_overloaded) {
        timeShift__role__overloaded();
    } else {
        timeShift__before__overloaded();
    }
}




	private boolean  stopRequestedAtCurrentFloor__before__twothirdsfull() {
		return env.getFloor(currentFloorID).hasCall()
				|| floorButtons[currentFloorID] == 1;
	}


	private boolean  stopRequestedAtCurrentFloor__role__twothirdsfull() {
		if (weight > maximumWeight*2/3) {
			return floorButtons[currentFloorID] == 1;
		} else return stopRequestedAtCurrentFloor__before__twothirdsfull();
	}


	private boolean
stopRequestedAtCurrentFloor__before__executivefloor() {
    if (FeatureSwitches.__SELECTED_FEATURE_twothirdsfull) {
        return stopRequestedAtCurrentFloor__role__twothirdsfull();
    } else {
        return stopRequestedAtCurrentFloor__before__twothirdsfull();
    }
}




	private boolean  stopRequestedAtCurrentFloor__role__executivefloor() { //executive
		if (isExecutiveFloorCalling() && !isExecutiveFloor(currentFloorID)) {
			return false;
		} else return stopRequestedAtCurrentFloor__before__executivefloor();
	}


	// alternative implementation: subclass of "ExecutiveFloor extends Floor"
	private boolean
stopRequestedAtCurrentFloor() {
    if (FeatureSwitches.__SELECTED_FEATURE_executivefloor) {
        return stopRequestedAtCurrentFloor__role__executivefloor();
    } else {
        return stopRequestedAtCurrentFloor__before__executivefloor();
    }
}





	private void continueInDirection(Direction dir) {
		currentHeading = dir;
		if (currentHeading == Direction.up) {
			if (env.isTopFloor(currentFloorID)) {
				//System.out.println("Reversing at Top Floor");
				currentHeading = currentHeading.reverse();
			}
		} else {
			if (currentFloorID == 0) {
				//System.out.println("Reversing at Basement Floor");
				currentHeading = currentHeading.reverse();
			}
		}
		if (currentHeading == Direction.up) {
			currentFloorID = currentFloorID + 1;
		} else {
			currentFloorID = currentFloorID - 1;
		}
	}




	private boolean isAnyLiftButtonPressed() {
		for (int i = 0; i < this.floorButtons.length; i++) {
			if (floorButtons[i] == 1) return true;
		}
		return false;
	}



	private boolean  stopRequestedInDirection__before__twothirdsfull (Direction dir, boolean respectFloorCalls, boolean respectInLiftCalls) {
		Floor[] floors = env.getFloors();
		if (dir == Direction.up) {
			if (env.isTopFloor(currentFloorID)) return false;
			for (int i = currentFloorID+1; i < floors.length; i++) {
				if (respectFloorCalls && floors[i].hasCall()) return true;
				if (respectInLiftCalls && this.floorButtons[i] == 1) return true;
			}
			return false;
		} else {
			if (currentFloorID == 0) return false;
			for (int i = currentFloorID-1; i >= 0; i--) {
				if (respectFloorCalls && floors[i].hasCall()) return true;
				if (respectInLiftCalls && this.floorButtons[i] == 1) return true;
			}
			return false;
		}
	}



	private boolean  stopRequestedInDirection__role__twothirdsfull (Direction dir, boolean respectFloorCalls, boolean respectInLiftCalls) {
		if (weight > maximumWeight*2/3 && isAnyLiftButtonPressed()) {
			if (verbose) System.out.println("over 2/3 threshold, ignoring calls from FloorButtons until weight is below 2/3*threshold");
			return stopRequestedInDirection__before__twothirdsfull(dir, false, respectInLiftCalls);
		} else return stopRequestedInDirection__before__twothirdsfull(dir, respectFloorCalls, respectInLiftCalls);
	}


	private boolean
stopRequestedInDirection__before__executivefloor(Direction dir, boolean respectFloorCalls, boolean respectInLiftCalls) {
    if (FeatureSwitches.__SELECTED_FEATURE_twothirdsfull) {
        return stopRequestedInDirection__role__twothirdsfull(dir, respectFloorCalls, respectInLiftCalls);
    } else {
        return stopRequestedInDirection__before__twothirdsfull(dir, respectFloorCalls, respectInLiftCalls);
    }
}




	private boolean  stopRequestedInDirection__role__executivefloor (Direction dir, boolean respectFloorCalls, boolean respectInLiftCalls) {
		if (isExecutiveFloorCalling()) {
			if (verbose) System.out.println("Giving Priority to Executive Floor");
			return ((this.currentFloorID < executiveFloor)  == (dir == Direction.up));

		} else return stopRequestedInDirection__before__executivefloor(dir, respectFloorCalls, respectInLiftCalls);
	}


	private boolean
stopRequestedInDirection(Direction dir, boolean respectFloorCalls, boolean respectInLiftCalls) {
    if (FeatureSwitches.__SELECTED_FEATURE_executivefloor) {
        return stopRequestedInDirection__role__executivefloor(dir, respectFloorCalls, respectInLiftCalls);
    } else {
        return stopRequestedInDirection__before__executivefloor(dir, respectFloorCalls, respectInLiftCalls);
    }
}




	private boolean anyStopRequested () {
		Floor[] floors = env.getFloors();
		for (int i = 0; i < floors.length; i++) {
			if (floors[i].hasCall()) return true;
			else if (this.floorButtons[i] == 1) return true;
		}
		return false;
	}



	public boolean buttonForFloorIsPressed(int floorID) {
		return this.floorButtons[floorID] == 1;
	}



	public Direction getCurrentDirection() {
		return currentHeading;
	}


	public Environment getEnv() {
		return env;
	}


	public boolean isEmpty() {
		return this.persons.isEmpty();
	}


	public boolean isIdle() {
		return !anyStopRequested();
	}



	@Override
	public String toString() {
		return "Elevator " + (areDoorsOpen() ? "[_]" :  "[] ") + " at " + currentFloorID + " heading " + currentHeading;
	}


	//TODO: implement the weight property in Persons in this Feature
	int weight;


	private static final int maximumWeight = 100;


	int executiveFloor = 4;


	public boolean isExecutiveFloor(int floorID) {return floorID == executiveFloor; }

	
	//private boolean isExecutiveFloor(Floor floor) {return floor.getFloorID() == executiveFloor; }
	public boolean isExecutiveFloorCalling() {
		for (Floor f : env.floors) 
			if (f.getFloorID() == executiveFloor && f.hasCall()) return true;
		return false;
	}

	
	private boolean blocked = false;


}
