module Tool.SCP.Common where

-- pretty YAML
import GTVM.SCP qualified as Scp
import Data.Yaml.Pretty qualified as Yaml.Pretty

scpPrettyYamlCfg :: Yaml.Pretty.Config
scpPrettyYamlCfg =
      Yaml.Pretty.setConfCompare Scp.scpSegFieldOrdering
    $ Yaml.Pretty.setConfDropNull True
    $ Yaml.Pretty.defConfig
