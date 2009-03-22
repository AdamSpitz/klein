"Run this script by going into the objects/ directory and running the command:    cat applications/klein/recreateKleinSnap.self | Self  "

'all2.self' _RunScript.
shell saveAs: 'AutoBuild.snap'.
'applications/klein/klein.self' runScript.
shell saveAs: 'kleinAutoBuild.snap'.

[ | hostName = 'localhost'. port = 9091. archName = 'ppc'. aProxy. aProcess. programExporter |

    programExporter:
      klein vmExporter copyForExporting:
        klein virtualMachines selfVM copyForArchitecture: archName.

    programExporter buildAndReportStatus: []
                                  IfFail: raiseError.
    
    klein debuggingProxyClient startServerOn: hostName Port: port.
    aProxy:  klein cachingDebuggingProxyClient copyForHost: hostName Port: port.
    aProcess: klein foreignProcess copyForDebuggingProxy: aProxy.

    programExporter copyForLaunch launchInForeignProcess: aProcess IfFail: raiseError.
    aProcess continue.
    'I can\'t believe this script worked.' printLine.
].
