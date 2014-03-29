
nodes = [livingRoom: "You are in the living room.\nA wizard is snoring loudly on the couch\n", 
         garden:     "You are in a beautiful garden.\nThere is a well in front of you\n", 
         attic:      "You are in the attic.\nThere is a giant welding torch in the corner\n"]

edges = [livingRoom: [garden:     ['west',       'door'], 
                      attic:      ['upstairs',   'ladder']],
         garden:     [livingRoom: ['east',       'door']], 
         attic:      [livingRoom: ['downstairs', 'ladder']]]

objectLocations = [livingRoom: ['whiskey', 'bucket'], 
                   garden: ['chain', 'frog']].withDefault { [] }

allowedCommands = ['look', 'walk', 'pickup', 'inventory']

location = 'livingRoom'

gameRepl()

def describeLocation(location, nodes) { nodes[location] }

def describePath(edge) { "There is a ${edge[1]} going ${edge[0]} from here." }

def describePaths(location, edges) { 
	edges[location].collect { k, v -> describePath(v) }.join('\n') + '\n' 
}

def describeObjects(location, objectLocations) {
	objectLocations[location].collect { obj -> "You see a $obj on the floor" }.join('\n')
}

def look() {
	describeLocation(location, nodes) +
	describePaths(location, edges) + 
	describeObjects(location, objectLocations)
}

def walk(direction) {
	def next = edges[location].find { loc, dirs -> direction == dirs[0] }?.key
	if (next) {
		location = next
		look()
	} else "You cannot go that way."
}

def pickup(object) {
	if (object in objectLocations[location]) {
		objectLocations[location] -= object
		objectLocations.body      += object
		"You are now carrying the $object"
	} else {
		"You cannot get that."
	}
}

def inventory() { "items- ${objectLocations.body.join(', ')}" }

def gameRepl() {
	while ((cmd = gameRead()) != 'quit') { println gameEval(cmd) + "\n" } 
}

def gameEval(cmd) {
	def t = cmd.tokenize(" ")
	if (!(t.head() in allowedCommands)) return "I do not know that command!"
		
	cmd = t.head() + (t.size() == 1 ? "() " : " ") + t.tail().collect { "'$it'" }.join(" ")

	Eval.me 'g', this, "g.$cmd"
}

def gameRead() { System.console().readLine(": ") }
