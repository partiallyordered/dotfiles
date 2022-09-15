package com.example.testproject;

import org.springframework.data.repository.CrudRepository;

import com.example.testproject.Company;

// This will be AUTO IMPLEMENTED by Spring into a Bean called userRepository
// CRUD refers Create, Read, Update, Delete

public interface CompanyRepository extends CrudRepository<Company, Integer> {

}
