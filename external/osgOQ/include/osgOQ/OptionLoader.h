//
// Copyright (C) 2007 Skew Matrix Software LLC (http://www.skew-matrix.com)
//
// This library is open source and may be redistributed and/or modified under  
// the terms of the OpenSceneGraph Public License (OSGPL) version 0.0 or 
// (at your option) any later version.  The full license is in LICENSE file
// included with this distribution, and on the openscenegraph.org website.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// OpenSceneGraph Public License for more details.
//

#ifndef OSGOQ_OPTION_LOADER_H
#define OSGOQ_OPTION_LOADER_H 1

#include <string>
#include <map>


// This class is designed to parse a text istream consisting of comment lines
//   and parameter / value(s) lines.
//
// Leading blanks before the start of the parameter name are skipped. Any
//   number of spaces or tabs can separate the parameter from its value(s).
//   The parameter name can't contain whitespace.
//
// A parameter/value line has the following syntax:
//   [whitespace]parameter<whitespace>value[whitespace]
// For example:
//   CachedLoad true
//
// Recognized Booleans:
//   true: "true", "1", "on", "y", "yes"
//   false: "false", "0", "off", "n", "no"
//
// Comments are defined as lines starting with '/' or '#'.
class OptionLoader
{
public:
    static OptionLoader* instance();

    // Load possibly multiple parameter/value(s) from the given istream.
    bool load( std::istream& in );

    bool getOption( const std::string& option, std::string& value ) const;
    bool getOption( const std::string& option, bool& value ) const;
    bool getOption( const std::string& option, int& value ) const;
    bool getOption( const std::string& option, float& value ) const;

private:
    OptionLoader();
    ~OptionLoader();

    typedef std::map<std::string,std::string> OptionMap;
    OptionMap _opts;

    static std::string trim( const std::string& str );

    static OptionLoader* _instance;
};


#endif
