class VelDict:
    """Parent class of the Jconf & Cluster dictionary data classes."""
    def __init__(self, preset = {}):
        """Initializes dictionary to Config if no presets are sent."""
        if len(preset) == 0:
            self.dictionary = self.ReadEntries()
        else:
            self.dictionary = {}
            for entry in preset:
                self.dictionary[entry] = preset[entry]

    def Add(self, name, value):
        name = self.UniqueName(name)
        self.dictionary[name] = value
        return name

    def Delete(self, name):
        del self.dictionary[name]

    def Length(self):
        """Returns the length of self.dictionary."""
        return len(self.dictionary)

    def __len__(self):
        """Returns the length of self.dictionary."""
        return self.Length()

    def GetNames(self):
        """Returns a sorted list of the entries' names."""
        nList = []
        for name in self.dictionary:
            nList.append(name)
        nList.sort(lambda x, y: cmp(x.lower(), y.lower()))
        return nList

    def ReadEntries(self):
        return {}      

    def UniqueName(self, name):
        return

    def WriteConfig(self, name, value):
        return
