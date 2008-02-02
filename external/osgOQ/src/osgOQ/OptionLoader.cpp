#include "osgOQ/OptionLoader.h"
#include <osg/Notify>
#include <osgDB/Registry>
#include <osgDB/FileNameUtils>
#include <osgDB/FileUtils>
#include <iostream>
#include <sstream>


OptionLoader* OptionLoader::_instance( NULL );

OptionLoader*
OptionLoader::instance()
{
    if (!_instance)
    {
        _instance = new OptionLoader();

        std::string envStr( "OSGOQ_CONFIG_FILE" );
        char* charPtr = getenv( envStr.c_str() );
        if (charPtr)
        {
            std::string configFile = std::string( charPtr );
            std::string fullName = osgDB::findDataFile( configFile );
            std::ifstream instr( fullName.c_str() );
            if (instr.good())
                _instance->load( instr );
            else
                osg::notify(osg::NOTICE) << "osgOQ: Can't load " << envStr << ": \"" << fullName << "\"." << std::endl;
        }
    }

    return _instance;
}

OptionLoader::OptionLoader()
{
}

OptionLoader::~OptionLoader()
{
}

bool
OptionLoader::load( std::istream& in )
{
    bool valid( true );
    int lineNum( 0 );
    while (in.good())
    {
        std::string raw;
        ++lineNum;
        std::getline( in, raw );
        std::string ln = trim( raw );
        if (ln.empty()) continue;
        if (ln[0] == '#') continue;
        if (ln[0] == '/') continue;

        std::string::size_type spIdx = ln.find_first_of( " \t" );
        if (spIdx == ln.npos)
        {
            // mapExt and toExt must be on the same line, separated by a space.
            osg::notify( osg::WARN) << "osgOQ: OptionLoader: line " << lineNum << ": Syntax error: missing space in \"" << raw << "\"." << std::endl;
            valid = false;
            continue;
        }

        const std::string option = trim( ln.substr( 0, spIdx ) );
        const std::string value = trim( ln.substr( spIdx+1 ) );
        _opts[ option ] = value;
        osg::notify( osg::DEBUG_INFO ) << "osgOQ: OptionLoader: Option >" << option << "< value: >" << value << "<." << std::endl;
    }

    return valid;
}

// If the option exists, return its string value and "true".
// Return "false" and do not modify the value otherwise.
bool
OptionLoader::getOption( const std::string& option, std::string& value ) const
{
    bool found( false );
    OptionMap::const_iterator it = _opts.find( option );
    if (it != _opts.end())
    {
        value = (*it).second;
        found = true;
    }
    return found;
}

// If the option exists and its value is a Boolean, return the Boolean value and "true".
// Return "false" and do not modify the value otherwise.
bool
OptionLoader::getOption( const std::string& option, bool& value ) const
{
    bool found( false );
    OptionMap::const_iterator it = _opts.find( option );
    if (it != _opts.end())
    {
        std::string optStr = (*it).second;
        if (osgDB::equalCaseInsensitive( optStr, "true" ) ||
            osgDB::equalCaseInsensitive( optStr, "on" ) ||
            osgDB::equalCaseInsensitive( optStr, "1" ) ||
            osgDB::equalCaseInsensitive( optStr, "y" ) ||
            osgDB::equalCaseInsensitive( optStr, "yes" ) )
        {
            found = true;
            value = true;
        }
        else if (osgDB::equalCaseInsensitive( optStr, "false" ) ||
            osgDB::equalCaseInsensitive( optStr, "off" ) ||
            osgDB::equalCaseInsensitive( optStr, "0" ) ||
            osgDB::equalCaseInsensitive( optStr, "n" ) ||
            osgDB::equalCaseInsensitive( optStr, "no" ) )
        {
            found = true;
            value = false;
        }
    }
    return found;
}

// If the option exists and its value is an integer, return the int value and "true".
// Return "false" and do not modify the value otherwise.
bool
OptionLoader::getOption( const std::string& option, int& value ) const
{
    bool found( false );
    OptionMap::const_iterator it = _opts.find( option );
    if (it != _opts.end())
    {
        std::istringstream iStr( (*it).second );
        int optInt;
        iStr >> optInt;
        if (!iStr.fail())
        {
            value = optInt;
            found = true;
        }
    }
    return found;
}

// If the option exists and its value is a float, return the float value and "true".
// Return "false" and do not modify the value otherwise.
bool
OptionLoader::getOption( const std::string& option, float& value ) const
{
    bool found( false );
    OptionMap::const_iterator it = _opts.find( option );
    if (it != _opts.end())
    {
        std::istringstream iStr( (*it).second );
        float optFloat;
        iStr >> optFloat;
        if (!iStr.fail())
        {
            value = optFloat;
            found = true;
        }
    }
    return found;
}

std::string
OptionLoader::trim( const std::string& str )
{
    if (!str.size()) return str;
    std::string::size_type first = str.find_first_not_of( " \t" );
    std::string::size_type last = str.find_last_not_of( "  \t\r\n" );
    if ((first==str.npos) || (last==str.npos)) return std::string( "" );
    return str.substr( first, last-first+1 );
}
