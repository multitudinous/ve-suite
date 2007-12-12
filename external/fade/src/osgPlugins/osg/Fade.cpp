#include <iostream>
#include "osg/Fade"

#include "osgDB/Registry"
#include "osgDB/Input"
#include "osgDB/Output"

using namespace osg;
using namespace osgDB;

// forward declare functions to use later.
bool Fade_readLocalData(Object& obj, Input& fr);
bool Fade_writeLocalData(const Object& obj, Output& fw);

// register the read and write functions with the osgDB::Registry.
RegisterDotOsgWrapperProxy g_FadeProxy
(
    new osg::Fade,
    "Fade",
    "Object Node Fade Group",
    &Fade_readLocalData,
    &Fade_writeLocalData
);

bool Fade_readLocalData(Object& obj, Input& fr)
{
    bool iteratorAdvanced = false;

    Fade& fade = static_cast<Fade&>(obj);

    if( fr.matchSequence( "FadePoints {"  ) )
    {
       fr += 2;
       while( !fr.eof() && !fr[0].isCloseBracket() )
       {
            double distance;
            double fadeVal;
            fr[0].getFloat(distance);
            fr[1].getFloat(fadeVal);
            fade.addFadePoint( distance, fadeVal );
            fr += 2;
       }
       ++fr;
       iteratorAdvanced = true;
    }
    return iteratorAdvanced;
}


bool Fade_writeLocalData(const Object& obj, Output& fw)
{
    const Fade& fade = static_cast<const Fade&>(obj);

    fw.indent() << "FadePoints {" << std::endl;
    fw.moveIn();

    const Fade::FadePointMap &fpm = fade.getFadePointMap();
    Fade::FadePointMap::const_iterator p;
    for( p = fpm.begin(); p != fpm.end(); p++ )
    {
        fw.indent() << p->first << "  " << p->second << std::endl;
    }
    fw.moveOut();
    fw.indent() << "}" << std::endl;

    return true;
}
