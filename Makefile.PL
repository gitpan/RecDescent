use ExtUtils::MakeMaker;


WriteMakefile(
    'NAME'	   => 'Parse::RecDescent',
    'VERSION_FROM' => 'RecDescent.pm',
    'PREREQ_PM'    => { 'Text::Balanced' => 1.21 }
);
