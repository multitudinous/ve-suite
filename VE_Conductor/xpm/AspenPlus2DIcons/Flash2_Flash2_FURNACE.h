#ifndef GETVESUITE_Flash2_Flash2_FURNACE_H
#define GETVESUITE_Flash2_Flash2_FURNACE_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Flash2_Flash2_FURNACE( void )
{
    unsigned char osgData[ 3522 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,79,0,79,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,254,40,162,138,0,43,224,31,248,43,15,252,162,207,254,10,87,223,254,48,7,246,200,227,254,237,215,226,53,125,253,95,3,127,193,86,160,154,235,254,9,115,255,0,5,37,182,182,134,91,139,139,143,216,27,246,196,130,8,32,141,166,154,121,166,253,158,62,34,199,20,48,196,138,90,89,89,217,85,84,2,88,176,0,18,104,3,241,43,254,9,133,255,0,4,245,253,129,60,127,224,125,30,243,199,127,176,239,236,127,227,107,169,116,191,50,75,175,23,126,205,31,5,252,73,113,36,159,101,118,47,36,218,199,130,166,103,147,118,14,75,103,62,166,191,36,63,224,188,95,177,231,236,145,240,131,224,223,197,141,79,225,47,236,183,251,57,124,47,212,180,239,9,120,150,231,79,212,126,29,124,17,248,105,224,155,235,11,136,52,203,151,130,123,59,191,13,248,98,214,75,89,209,194,178,50,50,178,178,130,8,32,26,253,235,255,0,130,81,252,94,248,77,164,248,7,68,139,85,248,161,240,239,76,149,116,141,165,47,252,109,225,187,38,80,182,205,27,141,151,26,154,16,119,225,79,78,78,56,53,248,249,255,0,7,6,248,203,194,30,35,248,33,241,142,63,15,120,175,195,122,235,159,6,120,168,44,122,54,187,165,234,140,79,246,69,228,135,139,11,169,9,249,3,55,251,163,112,59,121,175,134,161,95,18,243,59,58,179,116,212,246,187,183,196,142,153,40,114,43,70,237,175,242,63,187,90,40,162,190,228,230,10,40,162,128,10,231,252,89,226,207,10,248,11,194,190,38,241,215,142,188,77,225,255,0,5,248,39,193,126,31,214,124,89,227,31,24,248,179,89,211,188,57,225,95,9,248,87,195,154,117,206,177,226,31,19,120,155,196,58,197,204,54,154,15,135,236,52,139,59,203,171,219,219,169,162,182,181,182,181,146,121,228,72,145,152,127,50,159,183,31,252,28,221,240,43,225,180,158,35,248,101,255,0,4,250,248,107,172,126,217,255,0,21,173,134,181,163,15,139,82,91,107,126,25,253,149,124,35,173,91,31,27,120,127,251,82,195,196,209,218,46,179,251,65,125,131,196,250,47,132,245,8,45,252,49,111,166,248,51,197,222,31,241,35,73,161,252,85,181,190,140,69,95,203,151,237,21,241,79,254,10,19,255,0,5,31,241,132,62,42,253,176,252,99,241,59,226,86,139,166,248,137,124,85,224,207,130,58,7,134,181,223,10,126,206,191,14,110,173,53,63,20,95,248,102,95,6,252,37,208,109,126,197,113,226,61,38,15,25,248,135,71,211,188,79,226,1,175,248,242,77,23,80,109,35,87,241,62,165,5,186,76,112,171,136,165,69,123,242,215,183,245,234,52,155,217,31,212,231,237,167,255,0,7,48,252,5,240,29,223,136,126,24,126,192,94,6,155,246,186,248,153,104,218,182,139,63,198,141,94,77,79,193,255,0,178,143,130,181,88,27,198,254,31,125,99,75,241,1,133,53,223,218,65,180,239,18,104,158,22,212,162,180,240,141,173,143,131,188,89,225,239,17,52,250,47,197,75,43,168,138,175,243,233,241,67,226,151,237,79,251,126,248,214,203,198,255,0,182,159,197,239,16,252,84,211,180,239,17,31,18,120,39,224,228,22,109,225,47,128,63,12,245,107,45,87,196,215,254,31,187,240,15,193,93,22,229,180,196,215,244,43,127,23,120,131,74,210,60,93,175,29,123,226,17,210,245,31,236,173,71,198,186,165,180,113,58,240,191,12,127,101,31,136,246,98,209,173,126,23,120,254,224,197,111,1,105,32,240,87,136,231,11,186,70,101,22,248,211,221,101,149,164,192,220,62,69,101,12,237,52,129,29,191,65,62,29,254,205,191,23,173,97,133,109,190,17,252,74,102,102,17,198,208,120,11,197,44,170,29,27,124,171,255,0,18,128,103,140,41,144,33,102,62,99,180,173,128,228,99,197,197,102,78,113,106,19,84,226,251,111,109,63,175,233,154,198,9,91,171,59,207,130,158,24,182,208,44,33,75,104,34,68,72,146,222,53,141,16,121,48,195,106,85,202,42,194,130,8,29,154,16,161,118,171,136,144,149,220,51,92,63,199,191,3,233,222,42,138,238,59,219,88,166,183,187,55,49,207,28,138,210,45,195,199,230,171,47,148,8,45,39,146,33,80,122,229,87,203,100,108,200,159,105,120,39,224,23,198,200,45,173,227,139,224,207,197,18,191,100,102,253,215,195,207,23,176,59,228,70,64,207,253,143,150,144,68,170,14,119,17,228,158,78,208,107,15,199,255,0,0,254,51,220,44,194,79,132,31,20,17,150,68,219,35,120,7,197,139,182,54,183,140,179,34,29,49,68,172,178,164,36,245,56,78,8,93,213,225,123,88,251,75,169,164,239,254,69,217,246,63,35,126,4,254,213,223,182,207,252,19,118,245,45,255,0,100,47,139,103,78,248,77,108,215,211,15,217,127,226,182,141,168,124,80,253,155,35,243,34,241,93,228,176,248,127,225,212,62,34,211,53,31,131,241,167,136,252,107,172,248,130,225,190,29,107,158,15,95,16,235,114,195,125,226,161,226,72,224,143,79,147,250,84,253,141,127,224,228,111,216,183,227,189,229,167,128,255,0,106,203,57,191,96,79,140,18,181,202,133,248,197,226,237,55,94,253,155,181,223,45,188,105,169,47,252,35,63,181,28,122,78,147,165,104,13,7,134,124,51,163,181,226,120,255,0,74,240,12,151,58,231,139,109,188,61,225,148,241,61,204,102,226,79,195,191,137,63,179,143,197,137,214,91,139,143,133,31,18,21,216,59,20,255,0,132,31,197,25,70,143,50,74,209,180,154,110,4,128,201,36,168,202,78,229,103,143,97,82,164,254,124,252,83,253,148,252,119,117,22,162,46,254,25,248,201,25,211,115,71,115,224,255,0,17,67,111,58,196,178,9,17,195,88,130,135,201,98,173,30,4,138,133,149,75,197,182,39,250,12,38,100,210,140,106,77,77,119,235,211,239,51,148,23,77,25,254,157,116,87,249,134,126,203,63,182,7,252,20,191,254,9,129,36,90,95,236,185,227,79,20,205,240,119,73,55,205,63,236,201,241,191,194,190,38,248,141,251,57,197,28,203,226,187,168,215,194,222,15,159,83,211,117,95,133,14,222,39,241,222,179,175,222,159,2,107,222,18,26,246,189,37,181,223,139,6,173,109,105,246,41,191,172,63,216,155,254,14,88,253,137,63,104,139,155,79,1,254,212,214,87,191,240,79,255,0,140,178,73,125,19,89,124,110,241,45,158,165,251,61,107,50,90,127,194,93,169,202,190,26,253,166,228,209,180,125,51,68,22,190,25,208,116,6,188,62,60,210,124,6,46,245,191,25,90,104,30,21,62,41,156,11,169,125,138,85,233,214,87,132,175,229,212,205,166,183,71,240,103,251,16,255,0,200,147,224,191,251,21,124,51,255,0,163,210,191,161,15,130,223,126,47,247,160,255,0,211,148,181,252,247,254,196,63,242,36,248,47,254,197,95,12,255,0,232,244,175,232,67,224,183,223,139,253,232,63,244,229,45,124,246,61,222,172,253,127,200,210,27,63,83,245,95,224,239,252,123,217,127,215,11,79,214,236,230,191,71,126,26,127,171,211,191,221,183,255,0,219,138,252,226,248,59,255,0,30,246,95,245,194,207,255,0,74,205,126,142,252,52,255,0,85,167,127,187,111,255,0,183,21,243,56,157,239,219,254,1,209,13,159,169,247,71,129,127,227,214,215,218,24,241,248,45,198,43,140,248,129,247,15,214,79,253,37,90,236,252,11,255,0,30,182,223,245,197,63,244,11,138,227,62,32,125,195,245,147,255,0,73,22,188,246,218,158,142,218,175,208,179,224,127,137,223,234,36,250,77,255,0,164,43,95,154,127,26,58,222,127,215,91,223,253,35,21,250,89,241,59,253,68,159,73,191,244,133,107,243,79,227,71,91,207,250,235,125,255,0,164,98,189,76,59,126,237,250,181,215,208,206,167,67,242,75,227,231,252,123,107,191,245,206,243,249,88,215,243,135,251,114,255,0,200,15,197,95,245,253,15,254,158,252,51,95,209,231,199,207,248,246,215,127,235,157,231,242,177,175,231,15,246,229,255,0,144,31,138,191,235,250,31,253,61,248,102,190,163,46,191,180,134,189,191,67,25,252,44,250,83,246,33,255,0,145,39,193,127,246,42,248,103,255,0,71,165,127,66,31,5,190,252,95,239,65,255,0,167,41,107,249,239,253,136,127,228,73,240,95,253,138,190,25,255,0,209,233,95,208,135,193,111,191,23,251,208,127,233,202,90,195,29,252,90,158,191,228,40,108,253,79,213,127,131,191,241,239,101,255,0,92,44,255,0,244,172,215,232,239,195,79,245,90,119,251,182,255,0,251,113,95,156,95,7,127,227,222,203,254,184,89,255,0,233,89,175,209,223,134,159,234,180,239,247,109,255,0,246,226,190,111,19,215,250,236,116,67,103,234,125,209,224,95,248,245,182,255,0,174,41,255,0,160,92,87,25,241,3,238,31,172,159,250,72,181,217,248,23,254,61,109,191,235,138,127,232,23,21,198,124,64,251,135,235,39,254,146,45,121,207,227,249,162,207,129,254,39,127,168,147,233,55,254,144,173,126,105,252,104,235,121,255,0,93,111,191,244,140,87,233,103,196,239,245,18,125,38,255,0,210,21,175,205,63,141,29,111,63,235,173,247,254,145,138,244,240,255,0,99,213,126,134,117,58,31,146,95,31,63,227,219,93,255,0,174,119,159,202,198,191,156,63,219,151,254,64,126,42,255,0,175,232,127,244,247,225,154,254,143,62,62,127,199,182,187,255,0,92,239,63,149,141,127,56,127,183,47,252,128,252,85,255,0,95,208,255,0,233,239,195,53,245,57,119,199,31,85,250,24,207,225,103,210,159,177,15,252,137,62,11,255,0,177,87,195,63,250,61,43,250,16,248,45,247,226,255,0,122,15,253,57,75,95,203,143,236,175,251,81,124,11,248,113,225,143,12,105,222,51,241,207,246,53,238,157,160,104,118,55,144,31,12,248,195,81,16,221,89,202,30,226,47,51,74,240,252,233,38,208,164,238,82,202,127,132,183,21,251,29,240,195,254,10,147,251,8,248,117,227,58,199,199,79,177,133,48,100,255,0,194,178,248,199,114,6,203,231,149,219,253,23,225,235,147,251,162,27,241,192,231,129,56,218,21,165,82,110,20,165,43,190,222,130,139,74,58,179,250,87,248,59,255,0,30,246,95,245,194,207,255,0,74,205,126,142,252,52,255,0,87,167,127,187,111,255,0,183,53,252,198,124,53,255,0,130,219,255,0,193,48,244,8,173,87,86,253,166,190,198,99,138,217,28,127,194,152,253,160,238,8,100,184,103,97,155,111,133,14,15,203,207,28,30,149,246,175,129,255,0,224,225,79,248,35,254,140,150,75,169,126,215,127,103,48,172,94,103,252,88,47,218,126,109,133,60,253,216,48,124,21,108,253,225,211,57,6,188,26,248,12,108,221,163,133,156,191,237,215,228,109,9,69,38,155,254,180,63,168,111,2,255,0,199,173,183,253,113,79,253,2,226,184,207,136,31,112,253,95,255,0,73,86,191,22,252,39,255,0,7,46,127,193,19,52,219,123,116,189,253,181,60,150,72,214,50,7,236,227,251,90,200,21,149,103,4,126,235,224,57,207,222,31,157,115,62,49,255,0,131,147,63,224,138,250,154,127,160,254,217,222,112,37,200,207,236,235,251,88,70,70,235,96,131,30,119,192,165,231,32,254,85,194,242,204,193,206,235,7,83,167,217,101,243,195,249,145,246,151,196,239,245,18,125,38,255,0,210,21,175,205,63,141,29,111,63,235,173,247,254,145,138,241,223,29,127,193,127,191,224,145,218,212,101,116,223,218,207,237,7,247,160,39,252,40,127,218,102,44,230,209,99,3,55,31,6,83,31,62,70,120,233,147,95,15,252,77,255,0,130,206,127,193,53,252,67,246,159,236,143,218,68,94,25,37,186,41,255,0,22,123,227,221,184,34,91,97,28,121,251,87,194,228,198,91,35,219,25,60,87,161,71,1,141,139,141,240,211,73,53,209,249,25,206,81,118,179,191,244,142,83,227,231,252,123,107,191,245,206,243,249,88,215,243,135,251,114,255,0,200,15,197,95,245,253,15,254,158,252,51,95,170,191,23,191,224,165,63,177,79,138,32,213,147,66,248,211,246,214,186,91,145,110,63,225,92,252,89,181,223,191,236,160,0,111,60,9,24,82,76,114,125,238,155,125,193,175,197,15,218,175,227,159,194,223,137,26,87,136,45,188,23,226,129,173,92,94,221,199,45,170,127,98,120,139,79,18,34,234,154,21,195,54,117,125,30,221,87,247,54,119,39,13,183,253,94,0,201,92,253,30,2,133,104,78,46,116,220,86,155,163,41,181,102,175,171,63,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 3522; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

