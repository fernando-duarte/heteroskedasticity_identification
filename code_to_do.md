Error handling: Consider adding more tryCatch blocks around critical operations
Memory management: For very large simulations, consider using gc() periodically
Progress reporting: The current .progress = TRUE is good, but could add time estimates
Results saving: Consider saving intermediate results to disk for very long simulations