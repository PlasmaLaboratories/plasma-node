package org.plasmalabs.models

import grpc.health.v1.ServingStatus

/**
 * ServiceStatus
 * A container for the health status of a service.
 * @param service The name of the service. Ex: Node, Indexer, "" for all services.
 * @param status The health status of the service.
 */
final case class ServiceStatus(service: String, status: ServingStatus)
