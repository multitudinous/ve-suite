"""Parent class of Jconf & Cluster dictionaries."""

class VelDict:
    """Parent class of the Jconf & Cluster dictionary data classes.

    Functions:
        __init__([preset])
        Add(name, value)
        Delete(name)
        Length / __len__
        GetNames
        ReadEntries [empty]
        UniqueName(name) [empty]
        WriteConfig [empty]"""
    def __init__(self, preset = None):
        """Initializes dictionary to Config if no presets are sent."""
        if preset == None:
            self.dictionary = self.ReadEntries()
        else:
            self.dictionary = {}
            for entry in preset:
                self.dictionary[entry] = preset[entry]

    def Add(self, name, value):
        """Adds [name: value] to dictionary."""
        name = self.UniqueName(name)
        self.dictionary[name] = value
        return name

    def Delete(self, name):
        """Deletes name from dictionary."""
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
        """Reads entries from config and places them in dictionary."""
        return {}      

    def UniqueName(self, name):
        """Returns a unique name in dictionary for name."""
        return

    def WriteConfig(self):
        """Writes entries into config from self.dictionary. Empty."""
        """Writes dictionary to config."""
        return
