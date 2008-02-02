#ifndef DIGITAL_GAUGE_H
#define DIGITAL_GAUGE_H

// --- My Includes --- //
class Shaders;

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>
#include <osg/MatrixTransform>

namespace osgText
{
    class Text;
}

// --- C/C++ Libraries --- //
#include <string>

namespace display
{
class VE_USER_PLUGIN_EXPORTS DigitalGauge : public osg::MatrixTransform
{
public:
    DigitalGauge();
    DigitalGauge( std::string name );

    META_Node( display, DigitalGauge );

protected:
    ///Destructor
    virtual ~DigitalGauge();

    ///Copy constructor
    DigitalGauge( const DigitalGauge& digitalGauge, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    //Assignment operator
    DigitalGauge &operator=( const DigitalGauge &digitalGauge );

public:
    void Initialize();

    void UpdateText( double value );

    void SetPrecision( int precision );

    osgText::Text* GetNameText();

    osgText::Text* GetDigitalText();

private:
    int m_precision;
    
    std::string m_name;

    Shaders* m_shader;

    osg::ref_ptr< osgText::Text > m_nameText;
    osg::ref_ptr< osgText::Text > m_digitalText;
};
} // end display

#endif //DIGITAL_GAUGE_H
