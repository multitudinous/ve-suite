/*This class is designed to parse a text file that contains a property name, its value, and optional
 comments at the end of the line.
Notes:
-Maximum length of one line in the config file is 500 chars
-Leading blanks before the start of the property name are skipped
-The property name can't have any blanks
-There should be one or more blank spaces between the property name and the beginning of the value
-It is OK if there blank spaces included as part of the value
-Trailing blanks will be removed from the end of the value part
-The symbol '//' indicates the beginning of a comment.
-It is not necessary to put a blank space between the last char of the value and the start of the comment
-If a property is found, regardless of whether or not a value is found, then HasProperty() will return true
-The file parsing ends when the first blank line is found, or the end of file is reached

The following are examples of config file lines that are supported:

PROPERTY1 VALUE1
PROPERTY2  VALUE2
 PROPERTY3 VALUE3
PROPERTY4 VALUE4 //COMMENT HERE
PROPERTY5  VALUE5//COMMENT HERE
PROPERTY6  VALUE 6 //COMMENT HERE
*/

#ifndef OSGOQ_CONFIG_FILE_READER_H
#define OSGOQ_CONFIG_FILE_READER_H 1

#include <string>
#include <vector>


class ConfigFileReader
{
public:

	//CONSTRUCTOR
	ConfigFileReader();

	//UTILITIES
	bool SetConfigFileName( const std::string& aFileName );
	
	//ACCESSORS
	std::string GetValue( const std::string& aProperty );
	void GetValues( const std::string& aPropertyToFind, std::vector< std::string >& aListOfConfigValues );

	//UTILITIES
	bool HasProperty( const std::string& aPropertyToFind );

	static bool convertToInt( int& result, const std::string& in );
	static bool convertToBool( bool& result, const std::string& in );

	// Was this config file openable?
	bool valid() const { return _valid; }

private:

	//DATA MEMBERS
	std::string m_ConfigFileName;
	bool _valid;
};


#endif
