package com.example.testproject;

import org.springframework.data.repository.CrudRepository;

import com.example.testproject.Project;

// This will be AUTO IMPLEMENTED by Spring into a Bean called userRepository
// CRUD refers Create, Read, Update, Delete

public interface ProjectRepository extends CrudRepository<Project, Integer> {

}
