#include "osgOQ/ConfigFileReader.h"
#include <fstream>
#include <sstream>

ConfigFileReader::ConfigFileReader() :
	m_ConfigFileName( "" ),
	_valid( false )
{
}


//Use this function to set the filename, including path, for the config file to be used.  This function
// will make an attempt to open the file to ensure that it is found and can be read.  If it can't, then
// the function returns false.
bool
ConfigFileReader::SetConfigFileName( const std::string& aFileName )
{
	std::fstream inputStream;
	inputStream.open( aFileName.c_str() );

	if ( inputStream.is_open() )
	{
		m_ConfigFileName = aFileName;
		inputStream.close();
		_valid = true;
	}

	return _valid;
}


//Use this function to determine if the config file contains a property.  If the property name is found, then
// the function returns true, regardless of whether or not a value string follows the property name in the
// file.
bool
ConfigFileReader::HasProperty( const std::string& aPropertyToFind )
{
	if (!valid())
		return false;

	bool hasProperty = false;

	std::ifstream inputStream;
	inputStream.open( m_ConfigFileName.c_str() );

	if ( inputStream.is_open() )
	{
		bool continueReadingFile = true;

		char lineOfData[ 501 ];

		while ( continueReadingFile )
		{
			inputStream.getline( lineOfData, 500 );
			if (inputStream.fail())
				continueReadingFile = false;
			
			if ( continueReadingFile )
			{
				std::string propertyPart = "";
				bool isReadingProperty = false;
				bool continueReadingLine = true;
				int lastIndex = (int)( strlen( lineOfData ) ) - 1;
				for ( int i = 0; ( continueReadingLine == true ) && ( i <= lastIndex ); i++ )
				{
					char currentChar = lineOfData[ i ];

					if ( currentChar == '/' )
					{
						if ( i != lastIndex )
						{
							char nextChar = lineOfData[ i + 1 ];

							if ( nextChar == '/' )
							{
								//then this marks the beginning of a comment, so skip the rest of the line
								continueReadingLine = false;
							}
						}
					}

					if ( continueReadingLine )
					{
						if ( currentChar == ' ' )
						{
							if ( isReadingProperty )
							{
								//then we found the first blank space while reading a property, so this
								// marks the end of the property value
								continueReadingLine = false;
							}
						}
						else
						{
							if ( isReadingProperty )
							{
								propertyPart += currentChar;
							}
							else
							{
								if ( propertyPart.size() == 0 )
								{
									//then this is the first non-blank char found in the line.  This
									// will let you have blank spaces before the property name, if that
									// is necessary for whatever reason
									isReadingProperty = true;
									propertyPart += currentChar;
								}
							}
						}
					}
				}//end for loop

				if ( propertyPart == aPropertyToFind )
				{
					hasProperty = true;
					continueReadingFile = false;
				}
			}
		}
	}

	inputStream.close();

	return hasProperty;
}


std::string
ConfigFileReader::GetValue( const std::string& aPropertyToFind )
{
	std::string foundValue = "";
	if (!valid())
		return foundValue;

	std::ifstream inputStream;
	inputStream.open( m_ConfigFileName.c_str() );

	if ( inputStream.is_open() )
	{
		bool continueReadingFile = true;

		char lineOfData[ 501 ];

		while ( continueReadingFile )
		{
			inputStream.getline( lineOfData, 500 );
			if (inputStream.fail())
				continueReadingFile = false;
			
			if ( continueReadingFile )
			{
				std::string propertyPart = "";
				std::string valuePart = "";
				bool isReadingProperty = false;
				bool isReadingValue = false;
				bool continueReadingLine = true;
				int lastIndex = (int)( strlen( lineOfData ) ) - 1;
				for ( int i = 0; ( continueReadingLine == true ) && ( i <= lastIndex ); i++ )
				{
					char currentChar = lineOfData[ i ];

					if ( currentChar == '/' )
					{
						if ( i != lastIndex )
						{
							char nextChar = lineOfData[ i + 1 ];

							if ( nextChar == '/' )
							{
								//then this marks the beginning of a comment, so skip the rest of the line
								continueReadingLine = false;
							}
						}
					}

					if ( continueReadingLine )
					{
						if ( currentChar == ' ' )
						{
							if ( isReadingProperty )
							{
								//then we found the first blank space while reading a property, so this
								// marks the end of the property value
								isReadingProperty = false;
							}
							else if ( isReadingValue )
							{
								valuePart += currentChar;
							}
						}
						else
						{
							if ( isReadingProperty )
							{
								propertyPart += currentChar;
							}
							else if ( isReadingValue )
							{
								valuePart += currentChar;
							}
							else
							{
								//then we are either reading the first char in the line, or we just found the
								// first char in the property name after reading some leading blanks, or we
								// are in the blank spaces between the property name and value and just found
								// the first char of the value part.
								if ( propertyPart.size() == 0 )
								{
									//then this is the first non-blank char found in the line.  This
									// will let you have blank spaces before the property name, if that
									// is necessary for whatever reason
									isReadingProperty = true;
									propertyPart += currentChar;
								}
								else
								{
									isReadingValue = true;
									valuePart += currentChar;
								}
							}
						}
					}
				}//end for loop

				if ( propertyPart == aPropertyToFind )
				{
					//trim off trailing blanks before returning the value.  This will help in the case
					// where the user put some blanks before adding comments at the end of the line.
					//One problem could be if the user intentionally wants blanks at the end.  In that case,
					// the user should probably save their value with quotes and then parse the return value
					// on their own.
					int lastNonBlankChar = (int)( valuePart.find_last_not_of( ' ' ) );
					if ( lastNonBlankChar != -1 )
					{
						foundValue = valuePart.substr( 0, lastNonBlankChar + 1 );
					}
					else
					{
						foundValue = valuePart;
					}

					continueReadingFile = false;
				}
			}
		}
	}

	inputStream.close();

	return foundValue;
}


void
ConfigFileReader::GetValues( const std::string& aPropertyToFind, std::vector< std::string >& aListOfConfigValues )
{
	if ( valid() )
	{
		std::ifstream inputStream;
		inputStream.open( m_ConfigFileName.c_str() );

		if ( inputStream.is_open() )
		{
			bool continueReadingFile = true;

			char lineOfData[ 501 ];

			while ( continueReadingFile )
			{
				inputStream.getline( lineOfData, 500 );
				
				if (inputStream.fail())
				{
					continueReadingFile = false;
				}
				
				if ( continueReadingFile )
				{
					std::string propertyPart = "";
					std::string valuePart = "";
					bool isReadingProperty = false;
					bool isReadingValue = false;
					bool continueReadingLine = true;
					int lastIndex = (int)( strlen( lineOfData ) ) - 1;
					for ( int i = 0; ( continueReadingLine == true ) && ( i <= lastIndex ); i++ )
					{
						char currentChar = lineOfData[ i ];

						if ( currentChar == '/' )
						{
							if ( i != lastIndex )
							{
								char nextChar = lineOfData[ i + 1 ];

								if ( nextChar == '/' )
								{
									//then this marks the beginning of a comment, so skip the rest of the line
									continueReadingLine = false;
								}
							}
						}

						if ( continueReadingLine )
						{
							if ( currentChar == ' ' )
							{
								if ( isReadingProperty )
								{
									//then we found the first blank space while reading a property, so this
									// marks the end of the property value
									isReadingProperty = false;
								}
								else if ( isReadingValue )
								{
									valuePart += currentChar;
								}
							}
							else
							{
								if ( isReadingProperty )
								{
									propertyPart += currentChar;
								}
								else if ( isReadingValue )
								{
									valuePart += currentChar;
								}
								else
								{
									//then we are either reading the first char in the line, or we just found the
									// first char in the property name after reading some leading blanks, or we
									// are in the blank spaces between the property name and value and just found
									// the first char of the value part.
									if ( propertyPart.size() == 0 )
									{
										//then this is the first non-blank char found in the line.  This
										// will let you have blank spaces before the property name, if that
										// is necessary for whatever reason
										isReadingProperty = true;
										propertyPart += currentChar;
									}
									else
									{
										isReadingValue = true;
										valuePart += currentChar;
									}
								}
							}
						}
					}//end for loop

					if ( propertyPart == aPropertyToFind )
					{
						std::string currentLineValue = "";

						//trim off trailing blanks before returning the value.  This will help in the case
						// where the user put some blanks before adding comments at the end of the line.
						//One problem could be if the user intentionally wants blanks at the end.  In that case,
						// the user should probably save their value with quotes and then parse the return value
						// on their own.
						int lastNonBlankChar = (int)( valuePart.find_last_not_of( ' ' ) );
						if ( lastNonBlankChar != -1 )
						{
							currentLineValue = valuePart.substr( 0, lastNonBlankChar + 1 );
						}
						else
						{
							currentLineValue = valuePart;
						}

						aListOfConfigValues.push_back( currentLineValue );
					}
				}
			}
		}

		inputStream.close();
	}
}


// If the input string 'in' represents an integer,
//   return true and set 'result' to the integer value.
//   Otherwise return false.
bool
ConfigFileReader::convertToInt( int& result, const std::string& in )
{
	std::istringstream iStr( in );
	int value;
	iStr >> value;
	if (iStr.fail())
		return false;

	result = value;
	return true;
}

// If the input string 'in' represents a Boolean,
//   return true and set 'result' to the Boolean value.
//   Otherwise return false.
bool
ConfigFileReader::convertToBool( bool& result, const std::string& in )
{
	if ( (in.find_first_of( "Y" ) == 0) ||
		(in.find_first_of( "true" ) == 0) )
	{
		result = true;
		return true;
	}
	else if ( (in.find_first_of( "N" ) == 0) ||
		(in.find_first_of( "false" ) == 0) )
	{
		result = false;
		return true;
	}
	else
		return false;
}

