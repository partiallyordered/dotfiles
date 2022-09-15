package com.example.testproject;

import java.util.List;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;

@Entity // This tells Hibernate to make a table out of this class
public class Project {
  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  private Integer id;

  @OneToMany(targetEntity=Task.class, mappedBy="id", fetch=FetchType.LAZY)
  private List<Task> tasks;

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }
}

