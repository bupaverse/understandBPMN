This package is written to calculate several understandability metrics of a BPMN model using R and to test whether a relationship is possible in a BPMN model

See R documentation and paper on metric implementation for more information concerning this package. 

Metrics which can be calculated:

- Size (function size_bpmn())
- Number of empty sequence flows (function empty_sequence_flows(doc))
- Number of duplicate tasks (function duplicate_tasks())
- Number of pools (function pools())
- Number of data objects (function data_objects())
- Number of swimlanes (function swimlanes())
- Number of message flows (function message_flows())
- Density (function density_bpmn())
- Coefficient of network connectivity (function coefficient_network_connectivity())
- Average connector degree (function avg_connector_degree())
- Maximum connector degree (function max_connector_degree())
- Sequentiality (function sequentiality())
- Cyclicity (function cyclicity())
- Diameter (function diameter())
- Depth (function depth())
- Token split (function token_split())
- Control flow complexity (function control_flow_complexity())
- Connector mismatch (function connector_mismatch())
- Connector heterogeneity (function connector_heterogeneity())
- Separability (function separability())
- Structuredness (function structuredness())
- Cross Connectivity (function cross_connectivity())
- Extended cyclomatic metric of McCabe(function cyclomatic_metric())
