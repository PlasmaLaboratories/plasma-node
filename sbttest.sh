#! /bin/bash
sbt "testOnly bifrost.contract.ContractMethodSpec" 2>&1 | tee sbttest.log && sed -i -e "s/\(\|\[0m\|\[0m\[\|\[31m\)//g" sbttest.log